a = function(x = 999) {
    // public variables and methods
    let self = this
    self.x = 0
    self.initialize = function(x = 999) {
        self.x = x
        self
    }
    // private variables and methods
    let private = {}

    if (self.initialize) {
        self.initialize(x)
    }
}
a_instance = new a()
R.print(a_instance.x)
a_instance_2 = new a(123)
R.print(a_instance_2.x)
