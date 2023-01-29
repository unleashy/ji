namespace Ji

type Location =
    { Line: int
      Column: int }

    override this.ToString() : string = $"{this.Line}:{this.Column}"

module Location =
    let fromIndex (code: string) (index: int) : Location =
        let before = code[0 .. index - 1]

        // The amount of newlines before the span's text is its line.
        //   "ab\ncd\nef"
        //    1   2   3
        let line = (before |> String.filter (fun c -> c = '\n')).Length

        // The column is the offset between the last newline before the span and
        // the start of the span. Newline characters themselves count as part of
        // the same line as the characters before them.
        //   "abcd\nef\nghijkl"
        //    12345 123 123456
        let column = before[before.LastIndexOf("\n") + 1 ..].Length

        // + 1 adjust for 1-indexing
        { Line = line + 1; Column = column + 1 }
