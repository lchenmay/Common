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
| Maj_C___Min_A
| Maj_G___Min_E
| Maj_D___Min_B
| Maj_A___Min_F_Sharp
| Maj_E___Min_D_Flat
| Maj_B___Min_A_Flat
| Maj_F_Sharp___Min_E_Flat
| Maj_D_Flat___Min_B_Flat
| Maj__A_Flat___Min_F
| Maj_E_Flat___Min_C
| Maj_B_Flat___Min_G
| Maj_F___Min_D

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



