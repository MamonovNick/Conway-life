module Types where

type LifeWorld = (AliveCells, Control, Control)
type AliveCells = [(Int, Int)]
type Control = (Bool, Int, Int)
