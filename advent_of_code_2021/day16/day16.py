from dataclasses import dataclass, field
from enum import Enum
from functools import reduce
from math import prod
from operator import eq, gt, lt
from typing import NewType

RawPacket = NewType("RawPacket", str)


def read_raw(s: str) -> RawPacket:
    with open(s) as f:
        hexadecimal = f.read().strip()
        bitlength = 4 * len(hexadecimal)
        bs = bin(int(hexadecimal, 16))[2:]
        return RawPacket(("0" * (bitlength - len(bs))) + bs)


class TypeId(Enum):
    SUM = 0
    MUL = 1
    MIN = 2
    MAX = 3
    LIT = 4
    GT = 5
    LT = 6
    EQ = 7


@dataclass(frozen=True)
class Packet:
    version: int
    type_id: TypeId
    value: int = 0
    subpackets: list["Packet"] = field(default_factory=list)

    def sum_version_numbers(self) -> int:
        return self.version + sum(p.sum_version_numbers() for p in self.subpackets)

    def eval(self) -> int:
        return {
            TypeId.SUM: sum,
            TypeId.MUL: prod,
            TypeId.MIN: min,
            TypeId.MAX: max,
            TypeId.LIT: lambda _: self.value,
            TypeId.GT: lambda vs: int(reduce(gt, vs)),
            TypeId.LT: lambda vs: int(reduce(lt, vs)),
            TypeId.EQ: lambda vs: int(reduce(eq, vs)),
        }[self.type_id](p.eval() for p in self.subpackets)


@dataclass(frozen=False)
class Parser:
    raw_packet: RawPacket
    pointer: int = 0

    def read_int(self, step: int) -> int:
        n = int(self.raw_packet[self.pointer : self.pointer + step], 2)
        self.pointer += step
        return n

    def parse(self) -> Packet:
        version = self.read_int(3)
        if (type_id := TypeId(self.read_int(3))) == TypeId.LIT:
            value, more = 0, True
            while more:
                value <<= 4
                more = self.read_int(1) == 1
                value |= self.read_int(4)
            return Packet(version, type_id, value)
        else:
            subpackets = []
            if self.read_int(1) == 0:
                total_bit_length = self.read_int(15)
                current_spot = self.pointer
                while self.pointer - current_spot < total_bit_length:
                    subpackets.append(self.parse())
            else:
                num_sub_packets = self.read_int(11)
                while len(subpackets) < num_sub_packets:
                    subpackets.append(self.parse())
            return Packet(version, type_id, subpackets=subpackets)


if __name__ == "__main__":
    packet = Parser(read_raw("input.txt")).parse()
    print(f"Part 1: {packet.sum_version_numbers()}")
    print(f"Part 2: {packet.eval()}")
