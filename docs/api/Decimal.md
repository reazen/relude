# Decimal

## Overview

`Relude.Decimal` contains a type `t` which can represent an arbitrary precision numeric value, backed by an `int` mantissa and an `int` exponent.  This might be useful for representing currency values without loss in precision or floating point errors.  Because the mantissa and exponent are backed by ints, there is a fairly restrictive range of values that can be represented.

## Usage

TODO