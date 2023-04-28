Person2 = function(name, age) {
    // public variables and methods
    let self = this
    self.x = 10
    self.initialize = function(name, age) {
        private.name = name
        private.age = age
    }
    self.get_age = function() {
        R.print(private.age)
        R.print(private.get_age_plus_x())
    }
    // private variables and methods
    let private = {}
    private.age = null
    private.name = null
    private.get_age_plus_x = function() {
        return(R.add(private.age, self.x))
    }
    if (self.initialize) {
        self.initialize(name, age)
    }
}
elena = new Person2("Elena", 4)
elena.get_age()
