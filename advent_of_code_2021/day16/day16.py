from dataclasses import dataclass
from enum import Enum
from functools import reduce
from operator import mul
from typing import NewType

RawPacket = NewType("RawPacket", str)


def read_raw_packet(hexadecimal: str) -> RawPacket:
    bitlength = 4 * len(hexadecimal)
    bs = bin(int(hexadecimal, 16))[2:]
    return RawPacket(("0" * (bitlength - len(bs))) + bs)


def read_raw_packet_from_file(s: str) -> RawPacket:
    with open(s) as f:
        return read_raw_packet(f.read().strip())


class TypeId(Enum):
    SUM = 0
    PROD = 1
    MIN = 2
    MAX = 3
    LIT = 4
    GT = 5
    LT = 6
    EQ = 7


class LengthId(Enum):
    TOTAL_BIT_LENGTH = 0
    NUM_SUBPACKETS = 1


@dataclass(frozen=True)
class Packet:
    version: int
    type_id: TypeId

    def sum_version_numbers(self) -> int:
        return self.version

    def eval(self) -> int:
        raise ValueError("eval() called on a base Packet type.")


@dataclass(frozen=True)
class Literal(Packet):
    value: int

    def eval(self) -> int:
        return self.value


@dataclass(frozen=True)
class Operator(Packet):
    length_id: LengthId
    subpackets: list[Packet]

    def sum_version_numbers(self) -> int:
        return self.version + sum(p.sum_version_numbers() for p in self.subpackets)

    def eval(self) -> int:
        values = [p.eval() for p in self.subpackets]
        return {
            TypeId.SUM: sum(values),
            TypeId.PROD: reduce(mul, values, 1),
            TypeId.MIN: min(values),
            TypeId.MAX: max(values),
            TypeId.GT: int(int(values[0]) == max(values)),
            TypeId.LT: int(int(values[0]) == min(values)),
            TypeId.EQ: int(all(v == values[0] for v in values)),
        }[self.type_id]


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
        type_id = TypeId(self.read_int(3))
        if type_id == TypeId.LIT:
            value, more = 0, True
            while more:
                value <<= 4
                more = self.read_int(1) == 1
                value |= self.read_int(4)
            return Literal(version, type_id, value)
        else:
            subpackets = []
            length_id = LengthId(self.read_int(1))
            if length_id == LengthId.TOTAL_BIT_LENGTH:
                total_bit_length = self.read_int(15)
                current_spot = self.pointer
                while self.pointer - current_spot < total_bit_length:
                    subpackets.append(self.parse())
            else:
                num_sub_packets = self.read_int(11)
                while len(subpackets) < num_sub_packets:
                    subpackets.append(self.parse())
            return Operator(version, type_id, length_id, subpackets)


def parse(raw_packet: RawPacket) -> Packet:
    return Parser(raw_packet).parse()


def part_1(raw_packet: RawPacket) -> int:
    return parse(raw_packet).sum_version_numbers()


def part_2(raw_packet: RawPacket) -> int:
    return parse(raw_packet).eval()


if __name__ == "__main__":
    raw_packet = read_raw_packet_from_file("input.txt")
    for part in [part_1, part_2]:
        print(part(raw_packet))
