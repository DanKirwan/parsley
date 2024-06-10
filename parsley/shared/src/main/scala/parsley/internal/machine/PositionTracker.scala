package parsley.internal.machine

import scala.collection.mutable.ArrayBuffer

private [machine] class PositionTracker(val input: String) {
    // represents the first character offset for a new line index
    val newlineOffsets: ArrayBuffer[Int] = ArrayBuffer(0)
    var prevOffset = 0
    var prevLineIdx = 0
    var prevCol = 1

    /**
      * @param targetOffset offset we are looking to get position for
      * @return (line, col)
      */
    private [machine] def getPos(targetOffset: Int): (Int, Int) = {
        var lineIdx = prevLineIdx
        // Find the correct line first
        if(newlineOffsets(lineIdx) > targetOffset) {
            while(newlineOffsets(lineIdx) > targetOffset && lineIdx > 0) {
                lineIdx -= 1
            }
        } else {
            while (lineIdx + 1 < newlineOffsets.length && newlineOffsets(lineIdx + 1) <= targetOffset) {
                lineIdx += 1
            }
        }


        // If we're on the same line we started on we can use the cached column otherwise start again
        var (offset, col) = if(lineIdx == prevLineIdx && prevOffset <= targetOffset) 
            (prevOffset, prevCol) else (newlineOffsets(lineIdx), 1)



        while(offset < targetOffset) {
            input.charAt(offset) match {
                case '\t' => col = ((col + 3) & -4) | 1 // scalastyle:ignore magic.number
                case '\n' => 
                    lineIdx += 1; 
                    col = 1; 
                    if(lineIdx == newlineOffsets.length) {newlineOffsets += (offset + 1)}
                case _    => col += 1
                }
            offset += 1
        }

        prevLineIdx = lineIdx
        prevCol = col
        prevOffset = offset
        (lineIdx + 1, col)
    }
}
