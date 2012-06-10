## Distributed gen_server state example ##

Small example showing how to keep a backup of the state of a gen_server in a remote node.

___NOTE1: To me player =:= user ;)___

## Assumptions ##

1. A new node is connected to the cluster before start processing requests.
2. If a node is disconnected, is killed and don't process more requests.
3. At least 2 nodes needs to be alive for the system to work.

## Implementation details ##

This example represent a application server running in multiples nodes under a load balancer, the load balancer might query any of the application servers and the results shouldn't vary regarding of the nodes contacted.

A "interface" is provided to interact with the system, there are only 2 functions on the interface:

	interface:reward_player(Player/binary, Points/int).
	interface:local_players().

reward_player: Add points to a player, negative points are allowed.
local_players: Return a list with the players that are in this particular node.

There is a unique gen_server representing a user connected to the system, for each one of this users the system creates a "state keeper" on a different node.

When a user connects to the system the gen_server for this user is created on the local node, there are 3 possibilities for the backups.
* There is no backup running for this user: A new backup is created on a remote node.
* There is a backup running on a remote node: State is restored from the backup.
* The backup is running in the same node: State is restored from the backup, the backup is terminated and started again on a different node.

The reason to move the backup instead of creating the gen_server in a different node is that if sticky sessions are used, the load balancer would keep interacting with the "wrong" node, this would add some latency.

## Running the example ##

Open 3 (or more) terminals and start a node in each one of them:

	 ./start.sh app_node1
	 ...
	 ./start.sh app_nodeN

connect the nodes using ping or connect_node example from node 1:

	net_kernel:connect_node(app_node2@host).

reward players from any node:

	interface:reward_player(<<"player1">>, 5).
	5

reward a player with something different than an integer and see how the supervisor restart the gen_server and the state is preserved:

	interface:reward_player(<<"player1">>, a).
	CRASH!!
	interface:reward_player(<<"player1">>, 2).
	7

now feel free to explore and kill and restart nodes and keep rewarding players to see the magic ___don't forget to connect your nodes after a restart!___

## Disclaimer ##

This is just an exercise to learn more about erlang and otp, probably there are a lot of better ways of doing this.

This example was motivated by an interesting discussion on the [Zurich erlang user group](https://groups.google.com/forum/?fromgroups#!topic/zurich-erlang-user-group/WXroj2IPm8I).