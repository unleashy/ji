namespace Ji

type Span =
    { Code: string
      Index: int
      Length: int }

module Span =
    let emptyAt (code: string) (index: int) : Span =
        { Code = code
          Index = index
          Length = 0 }

    let ofSlice (code: string) (start: int) (finish: int) : Span =
        assert (start >= 0)
        assert (start <= code.Length)
        assert (finish >= 0)
        assert (finish <= code.Length)
        assert (start <= finish)

        { Code = code
          Index = start
          Length = finish - start }

    let concat (left: Span) (right: Span) : Span =
        assert (left.Code = right.Code)

        let start = min left.Index right.Index
        let finish = max (left.Length + left.Index) (right.Length + right.Index)

        ofSlice left.Code start finish

[<AutoOpen>]
module SpanOperations =
    let inline (++) left right = Span.concat left right

type Location =
    { Line: int
      Column: int }

    override this.ToString() : string = $"{this.Line}:{this.Column}"

module Location =
    let ofIndex (code: string) (index: int) : Location =
        let before = code[0 .. index - 1]

        // The amount of newlines before the span's text is its line.
        //   "ab\ncd\nef"
        //    1   2   3
        let line = before |> String.filter ((=) '\n') |> String.length

        // The column is the offset between the last newline before the span and
        // the start of the span. Newline characters themselves count as part of
        // the same line as the characters before them.
        //   "abcd\nef\nghijkl"
        //    12345 123 123456
        let column = before[before.LastIndexOf("\n") + 1 ..] |> String.length

        // + 1 adjust for 1-indexing
        { Line = line + 1; Column = column + 1 }

    let ofSpan (span: Span) : Location = ofIndex span.Code span.Index
