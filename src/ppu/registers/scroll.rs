pub struct ScrollRegister {
    scroll_x: u8,
    scroll_y: u8,
    latch: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
            latch: false,
        }
    }

    pub fn update(&mut self, data: u8) {
        if self.latch {
            self.scroll_x = data;
        } else {
            self.scroll_y = data;
        }

        self.latch = !self.latch;
    }

    pub fn reset_latch(&mut self) {
        self.latch = false;
    }
}
