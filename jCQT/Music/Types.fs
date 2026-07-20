namespace jCQT.Music.Types

type NoteTime = 
| Whole
| Half
| Quart
| Oct
| Sixteenth
| ThirtySecond

type TimeSignature = {
beats: int
note: NoteTime }

type Key =
| C
| G
| D
| A
| E
| B
| F_sharp
| D_flat
| A_flat
| E_flat
| B_flat
| F

type Clef =
| Treble
| Bass
| Alto
| Tenor

type Note = {
clef: Clef
noteTime: NoteTime
dot: bool
pitch: int }

type Voice = {
index: int
notes: Note[] }

type GrandStaffMeasure = {
mutable timeSignature: TimeSignature
mutable key: Key 
voices: Voice[] }



