//! # Monero network crawler
//!
//! A simple tool to find all the reachable nodes on the Monero P2P network. It works by recursively connecting to
//! every peer we are told about in a peer list message, starting by connecting to the seed nodes.
use std::{
    collections::HashSet,
    convert::Infallible,
    fs::OpenOptions,
    io::Write,
    net::SocketAddr,
    sync::{LazyLock, OnceLock},
    task::Poll,
    time::Duration,
};

use dashmap::DashSet;
use futures::{future::BoxFuture, stream, FutureExt};
use tokio::{
    sync::{mpsc, Semaphore},
    time::timeout,
};
use tower::{make::Shared, util::MapErr, Service, ServiceExt};
use tracing::error;
use tracing_subscriber::{filter::LevelFilter, FmtSubscriber};

use cuprate_p2p_core::{
    client::{
        handshaker::builder::{DummyCoreSyncSvc, DummyProtocolRequestHandler},
        ConnectRequest, Connector, HandshakerBuilder, InternalPeerID,
    },
    services::{AddressBookRequest, AddressBookResponse},
    BroadcastMessage, ClearNet, NetZoneAddress, Network, NetworkZone,
};
use cuprate_wire::{common::PeerSupportFlags, BasicNodeData};

use clap::Parser;

/// A simple tool to find all the reachable nodes on the Monero P2P network. It works by recursively connecting to
/// every peer we are told about in a peer list message, starting by connecting to the seed nodes.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
   /// Collect peer lists provided by other nodes and write to peer_list.txt
   #[arg(short, long, action = clap::ArgAction::SetTrue)] // Defines -c/--collect-peer-lists flag
   collect_peer_lists: bool,
}

/// A set of all node's [`SocketAddr`] that we have successfully connected to.
static SCANNED_NODES: LazyLock<DashSet<SocketAddr>> = LazyLock::new(DashSet::new);

/// The [`Connector`] service to make outbound connections to nodes.
static CONNECTOR: OnceLock<
    Connector<
        ClearNet,
        AddressBookService,
        DummyCoreSyncSvc,
        MapErr<Shared<DummyProtocolRequestHandler>, fn(Infallible) -> tower::BoxError>,
        fn(InternalPeerID<<ClearNet as NetworkZone>::Addr>) -> stream::Pending<BroadcastMessage>,
    >,
> = OnceLock::new();

/// The channel that is used to communicate a successful connection.
static BAD_PEERS_CHANNEL: OnceLock<mpsc::Sender<(SocketAddr, bool)>> = OnceLock::new();

/// A [`Semaphore`] to limit the amount of concurrent connection attempts so we don't overrun ourself.
static CONNECTION_SEMAPHORE: Semaphore = Semaphore::const_new(1);

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    FmtSubscriber::builder()
        .with_max_level(LevelFilter::DEBUG)
        .init();

    let handshaker = HandshakerBuilder::<ClearNet>::new(BasicNodeData {
        my_port: 0,
        network_id: Network::Mainnet.network_id(),
        peer_id: rand::random(),
        support_flags: PeerSupportFlags::FLUFFY_BLOCKS,
        rpc_port: 0,
        rpc_credits_per_hash: 0,
    })
    .with_address_book(AddressBookService)
    .build();

    // Create and set the CONNECTOR global.
    let connector = Connector::new(handshaker);
    let _ = CONNECTOR.set(connector.clone());

    // Create and set the BAD_PEERS_CHANNEL global.
    let (bad_peers_tx, mut bad_peers_rx) = mpsc::channel(508);
    BAD_PEERS_CHANNEL.set(bad_peers_tx).unwrap();

    // start by connecting to the seed nodes
    [
        "176.9.0.187:18080",
        "88.198.163.90:18080",
        "66.85.74.134:18080",
        "51.79.173.165:18080",
        "192.99.8.110:18080",
        "37.187.74.171:18080",
        "77.172.183.193:18080",
    ]
    .into_iter()
    .for_each(|ip| {
        tokio::spawn(check_node(ip.parse().unwrap()));
    });

    let mut bad_peers = HashSet::new();
    let mut bad_peers_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("bad_peers.txt")
        .unwrap();

    let mut good_peers_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("good_peers.txt")
        .unwrap();

    loop {
        let (peer, peer_bad) = bad_peers_rx.recv().await.unwrap();

        if peer_bad {
            error!("Found bad peer: {peer:?}");
            if !bad_peers.insert(peer) {
                continue;
            }

            bad_peers_file
                .write_fmt(format_args!("peer: {peer:?}, \n"))
                .unwrap();
        } else {
            good_peers_file
                .write_fmt(format_args!("peer: {peer:?}, \n"))
                .unwrap();
        }
    }
}

/// Check a node is reachable, sending the address down the [`BAD_PEERS_CHANNEL`] if it is.
async fn check_node(addr: SocketAddr) -> Result<(), tower::BoxError> {
    // Acquire a semaphore permit.
    let _guard = CONNECTION_SEMAPHORE.acquire().await.unwrap();

    if Cli::parse().collect_peer_lists {
        let mut peer_list_file = OpenOptions::new()
        .create(true)
            .append(true)
            .open("peer_list.txt")
            .unwrap();
    
        peer_list_file
            .write_fmt(format_args!("connected_node: {addr:?}, \n"))
            .unwrap();
    }

    // Grab the connector from the `CONNECTOR` global
    let mut connector = CONNECTOR.get().unwrap().clone();

    // Attempt to connect and handshake, with a timeout.
    let mut client = timeout(
        Duration::from_secs(5),
        connector
            .ready()
            .await?
            .call(ConnectRequest { addr, permit: None }),
    )
    .await??;

    // The proxy check has been removed - all peers reachable are good.
    let bad = false;

    BAD_PEERS_CHANNEL.get().unwrap().send((addr, bad)).await?;

    Ok(())
}

/// An address book service used in the [`CONNECTOR`] that just calls [`check_node`] on each peer in an
/// incoming peer list and does not actually track peer addresses.
#[derive(Clone)]
pub struct AddressBookService;

impl Service<AddressBookRequest<ClearNet>> for AddressBookService {
    type Error = tower::BoxError;
    type Response = AddressBookResponse<ClearNet>;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: AddressBookRequest<ClearNet>) -> Self::Future {
        async {
            match req {
                AddressBookRequest::IncomingPeerList(peers) => {

                    if Cli::parse().collect_peer_lists {
                        
                        let mut peer_list_file = OpenOptions::new()
                          .create(true)
                          .append(true)
                          .open("peer_list.txt")
                          .unwrap();
                        for mut peer in peers {
                            peer.adr.make_canonical();
                            let mut peer_adr = peer.adr;
                            peer_list_file
                              .write_fmt(format_args!("peer: {peer_adr:?}, \n"))
                              .unwrap();

                            if SCANNED_NODES.insert(peer.adr) {
                                tokio::spawn(async move {
                                    if check_node(peer.adr).await.is_err() {
                                        SCANNED_NODES.remove(&peer.adr);
                                    }
                                });
                            }
                        }
                    } else {
                    
                      for mut peer in peers {
                          peer.adr.make_canonical();
                          if SCANNED_NODES.insert(peer.adr) {
                              tokio::spawn(async move {
                                  if check_node(peer.adr).await.is_err() {
                                      SCANNED_NODES.remove(&peer.adr);
                                  }
                              });
                          }
                       }
                    }

                    Ok(AddressBookResponse::Ok)
                }
                AddressBookRequest::NewConnection { .. } => Ok(AddressBookResponse::Ok),
                AddressBookRequest::GetWhitePeers(_) => Ok(AddressBookResponse::Peers(vec![])),
                AddressBookRequest::TakeRandomWhitePeer { .. }
                | AddressBookRequest::TakeRandomGrayPeer { .. }
                | AddressBookRequest::TakeRandomPeer { .. }
                | AddressBookRequest::PeerlistSize
                | AddressBookRequest::ConnectionCount
                | AddressBookRequest::SetBan(_)
                | AddressBookRequest::GetBan(_)
                | AddressBookRequest::GetBans
                | AddressBookRequest::ConnectionInfo => Err("no peers".into()),
            }
        }
        .boxed()
    }
}
