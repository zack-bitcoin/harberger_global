harberger global README
===========

Harberger taxes
===============

Harberger taxes are a kind of continuous auction for allocating resources efficiently.

It has nice properties:
* whoever can produce the most value from controlling a resource, they are the ones who should have control.
* If the owner of a resource changes, the previous owner is compensated according to how much that resource is worth to them.
* Everyone who currently controls a resource, they are incentivized to honestly report the value of that resource to them.

It has a interesting side effect. In order for current controllers of resources to report honestly, some money needs to get burned. Burning this money creates a cryptocurrency, and the market cap of this cryptocurrency is about the same as the value of all resources held in the harberger registry.

Merkle Trees
===============

In order to store a land registry in a merkle tree, we need a trustless way to be sure that the same land hasn't been registered to more than one person.
So, the act of verifying a merkle proof needs to also give evidence that this land is owned uniquely.

With data merkle trees are easy. Each data can be stored by the hash of that data, and the hash of each unique data is unique.

The way to make land unique is that each node in the Merkle tree should specify a great circle that divides the earth in 2 halves. The left branch of the tree can hold information about the left hemisphere of the planet, and the right branch holds information about the right hemisphere. This way we can be certain that each side of the tree holds non-overlapping regions of land.

[here is a math library for converting gps coordinates into rational spherical coordinates, converting rational spherical coordinates back to gps coordinates, and connecting rational coordinates into rational great circles](/rationals.erl)

What we still need
===================

We need a second merkle tree to keep track of bids. Where people make offers to buy regions of land.

We need a way for someone to post a combination of bids, and show that the resulting change of ownerships will result in the land having higher value, and this makes the ownerships all change.

We need a way for someone to show that reorganizing the merkle tree of land ownership can make it more balanced, so merkle proofs can be shorter on average.

