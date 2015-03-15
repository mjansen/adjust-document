crop_ppm :: Int -> Int -> BS.ByteString -> Int -> Int -> BS.ByteString
crop_ppm fullw fullh src triml trimr trimt trimb =
    let x_offset = triml
        x_width  = trimr - triml + 1
        y_offset = trimt
        y_height = trimb - trimt + 1
    in BS.concat [ BS.
