Person2 = function(name, age) {
    // public variables and methods
    this.x = 10
    this.initialize = function(name, age) {
        private.name = name
        private.age = age
    }
    this.get_age = function() {
        R.print(private.age)
        R.print(private.get_age_plus_x())
    }
    // private variables and methods
    let that = this, private = {}
    private.age = null
    private.name = null
    private.get_age_plus_x = function() {
        return(R.add(private.age, that.x))
    }
    if (this.initialize) {
        this.initialize(name, age)
    }
}
elena = new Person2("Elena", 4)
elena.get_age()
