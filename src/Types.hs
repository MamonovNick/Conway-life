module Types where

type LifeWorld = (AliveCells, Control, Control) -- Second Control for backup current config
type AliveCells = [(Int, Int)]
type Control = (Bool, Int, Int)
