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

[here is a math library for converting gps coordinates into rational spherical coordinates, converting rational spherical coordinates back to gps coordinates, and connecting rational coordinates into rational great circles](/globe.erl)

[here is a math library for accepting a list of great circle constraints, and computing the points on the corners of the enclosed region](/spherical_trig.erl)


Key functions
==============

This function converts gps coordinates into points on the globe.
`globe:gps_to_point({23.5, 196.3}).`

We also have the reverse
`globe:point_to_gps({point, 400, 4929, -2838}).`

Given two points on the globe, how to find the best possible estimate of the great circle that connects them.
`globe:estimate_line(P1, P2).`

Given a list of great circles from the verkle proof, how to find the region enclosed by those great circles?
`Points = spherical_trig:region(GreatCircles).`

Given a list of points that walk around the border of a title, calculate the area of the title.
`Area = spherical_trig:area2(Points).`
`Area_in_square_meters = rat:to_float(Area) * globe:radius() * globe:radius().`

check if the title is too squished to be valid.
`spherical_trig:too_squished(Area, Points).`
