#!/bin/bash

# constant parameters
m=0
atomic_charge=1
d_r=0.1
r_max=100.0

# parameter sets
n_basis_set="1 2 4 8 16 32 64 128"
l_set="0 1 2 3"
alpha_set="0.5 0.75 1.0 1.25 1.5"

# compile
make hydrogenic_atom

# run jobs
for n_basis in ${n_basis_set} ; do
    for l in ${l_set} ; do
        for alpha in ${alpha_set} ; do
            bin/hydrogenic_atom ${l} ${m} ${alpha} ${atomic_charge} \
                                ${n_basis} ${d_r} ${r_max}

        done
    done
done
