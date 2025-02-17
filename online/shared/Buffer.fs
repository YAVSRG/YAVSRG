namespace Interlude.Web.Shared

#nowarn "3370"

open System

[<Struct>]
type Buffer =
    | Empty
    | Header_Size_1 of byte
    | Header_Size_2 of msb: byte * lsb: byte
    | Reading_Buffer of remaining_size: int * kind: byte * buffer: byte array

module Buffer =

    (*
        Packets have a 3 byte header
        - First byte: MOST SIGNIFICANT (this is BIG ENDIAN) bits of the length of the packet body
        - Second byte: LEAST SIGNFICIANT bits of the length of the packet body
        - Third byte: packet 'kind' - See Packets.fs
        Followed by body bytes, the same number as represented in those first 2 bytes

        After that the next byte is the first of the header for another packet
    *)

    // This is a consequence of only having 2 bytes to encode the length, 65535 is the biggest representable number
    let MAX_PACKET_SIZE = 65535

    let handle (buffer: Buffer ref, data: byte array, offset: int64, size: int64, packet: byte * byte array -> unit) =
        let mutable offset = int offset
        let size = int size

        while offset < size do

            match !buffer with
            | Empty ->
                buffer := Header_Size_1 data.[offset]
                offset <- offset + 1
            | Header_Size_1 msb ->
                buffer := Header_Size_2(msb, data.[offset])
                offset <- offset + 1
            | Header_Size_2(msb, lsb) ->
                let packet_length = (int msb <<< 8) + int lsb

                if packet_length <= 0 then
                    buffer := Empty
                    packet (data.[offset], [||])
                else
                    buffer
                    := Reading_Buffer(packet_length, data.[offset], Array.zeroCreate (int packet_length))

                offset <- offset + 1

            | Reading_Buffer(remaining_size, kind, b) ->
                let amount_to_copy = min (size - offset) remaining_size
                Buffer.BlockCopy(data, offset, b, b.Length - remaining_size, amount_to_copy)
                let new_remaining = remaining_size - amount_to_copy
                offset <- offset + amount_to_copy

                if new_remaining = 0 then
                    buffer := Empty
                    packet (kind, b)
                else
                    buffer := Reading_Buffer(new_remaining, kind, b)

    let packet_bytes (kind: byte, data: byte array) =
        if data.Length > MAX_PACKET_SIZE then
            failwithf "Packet data exceeded maximum size"

        let result = Array.zeroCreate (data.Length + 3)
        Buffer.BlockCopy(data, 0, result, 3, data.Length)
        result.[0] <- data.Length >>> 8 |> byte
        result.[1] <- data.Length |> byte
        result.[2] <- kind
        result