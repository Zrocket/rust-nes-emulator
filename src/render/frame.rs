pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256 * 2;
    const HIGHT: usize = 240;

    pub fn new() -> Self {
        Frame {
            data: vec![0; (Frame::WIDTH) * (Frame::HIGHT) * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2;
        }
    }

    pub fn get_pixel_base(&mut self, x: usize, y: usize) -> usize {
        y * 3 * Frame::WIDTH + x * 3
    }

    pub fn get_pixel(&mut self, x: usize, y: usize) -> (u8, u8, u8) {
        let base = y * 3 * Frame::WIDTH + x * 3;
        if base + 2 < self.data.len() {
            return (self.data[base], self.data[base + 1], self.data[base + 2]);
        }
        (0, 0, 0)
    }


    pub fn merge(&mut self, second_frame: &Frame) {
        for i in (0..self.data.len()).step_by(3) {
            if self.data[i] == 0 && self.data[i+1] == 0 && self.data[i+2] == 0 {
                self.data[i] = second_frame.data[i];
                self.data[i+1] = second_frame.data[i+1];
                self.data[i+2] = second_frame.data[i+2];
            }
        }
    }
}
