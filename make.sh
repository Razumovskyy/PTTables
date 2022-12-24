#!/bin/bash

read -p "Please provide line shape: " shape

find . -name '*.out' -type f -exec rm {} \;

gfortran p1.f90 p0.f90 p2.f90 p3.f90 $shape.f90 -o run.out