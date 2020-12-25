Person = function(name, age) {
    // public variables and methods
    let self = this
    self.name = null
    self.age = null
    self.initialize = function(name, age) {
        self.name = name
        self.age = age
    }
    // private variables and methods
    let that = this, private = {}
    
    if (self.initialize) {
        self.initialize(name, age)
    }
}
eva = new Person("Eva", 6)
R.print(eva.name)
R.print(eva.age)
