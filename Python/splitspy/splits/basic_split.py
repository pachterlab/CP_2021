# basic_split.py
"""Basic implementation of a split

LICENSE: This is open-source software released under the terms of the
GPL (http://www.gnu.org/licenses/gpl.html).
"""

import copy

__author__ = "Daniel H. Huson"

from typing import Tuple, Set


class Split:
    def __init__(self, part1: [int], part2: [int], weight: float = 1.0):
        self.__part1 = set(part1)
        self.__part2 = set(part2)
        self.weight = weight

    def __str__(self):
        return f'{self.part1()} {self.weight: .8f}'

    def part1(self) -> Set[int]:
        return self.__part1

    def part2(self) -> Set[int]:
        return self.__part2

    def part_in(self, taxon: int) -> Set[int]:
        if taxon in self.part1():
            return self.part1()
        else:
            return self.part2()

    def part_not_in(self, taxon: int) -> Set[int]:
        if taxon not in self.part1():
            return self.part1()
        else:
            return self.part2()

    def separates(self, tax1: int, tax2: int) -> bool:
        return (tax1 in self.part1()) != (tax2 in self.part1())

    def separates_sets(self, tax1: int, tax2: [int]) -> bool:
        """
        Parameters :
        tax1 : Taxa in cycle
        tax2 : Set of taxa in cycle
        Returns :
        Bool is tax1 separated from all of tax2
        """
        part12Split = (tax1 in self.part1()) and (all(i in self.part2() for i in tax2))
        part21Split = (tax1 in self.part2()) and (all(i in self.part1() for i in tax2))

        return (part12Split or part21Split)

    def size(self) -> int:
        return min(len(self.part1()), len(self.part2()))

    def is_trivial(self) -> bool:
        return self.size() == 1

    def get_weight(self) -> float:
        return self.weight

    def set_weight(self, weight: float) -> None:
        self.weight = weight

    def deepcopy(self):
        return copy.deepcopy(self)

    def interval(self, cycle: [int]) -> Tuple[int, int]:
        p = self.part_not_in(cycle[1])
        a = 0
        b = 0
        for i in range(1, len(cycle)):
            if cycle[i] in p:
                if a == 0:
                    a = i
                b = i
        return a, b


def compatible(splits: [Split]) -> bool:
    p1 = [s.part1() for s in splits]
    p2 = [s.part2() for s in splits]

    for s in range(0, len(splits)):
        for t in range(s + 1, len(splits)):
            if not (p1[s].isdisjoint(p1[t]) or p1[s].isdisjoint(p2[t]) or p2[s].isdisjoint(p1[t])
                    or p2[s].isdisjoint(p2[t])):
                return False
    return True


def split_dist(n_tax: int, splits: [Split]) -> [[float]]:
    mat = [[0] * (n_tax + 1)]

    for a in range(1, n_tax + 1):
        row = [0]
        for b in range(1, n_tax + 1):
            dist = 0
            for sp in splits:
                if sp.separates(a, b):
                    dist += sp.weight
            row.append(dist)
        mat.append(row)
    return mat

def split_dist_sets(tax1: [int], tax2: [int], splits: [Split]) -> [[float]]:
    """
    Parameters :
    tax1 : Set of taxa in cycle
    tax2 : Set of taxa in cycle to compare each tax1 against
    splits: All splits in nnet split system
    Returns :
    Distances from each taxa input to split from all taxa in tax2 
    """

    num_tax = len(tax1)
    vec = [0]*num_tax
    i = 0

    for a in tax1:
        dist = 0
        # Add all splits were tax1 in part1 and all tax2 in part2
        for sp in splits:
            if sp.separates_sets(a, tax2):
                dist += sp.weight

        vec[i] = dist
        i += 1
    
    return vec


def cyc_split(cycle: [int], pos1: int, pos2: int, wgt: float) -> Split:
    p1 = []
    p2 = []

    for i in range(1, len(cycle)):
        if i < pos1 or i > pos2:
            p1.append(cycle[i])
        else:
            p2.append(cycle[i])
    return Split(p1, p2, wgt)
