Person = function(name, age) {
    // public variables and methods
    this.name = null
    this.age = null
    this.initialize = function(name, age) {
        this.name = name
        this.age = age
    }
    // private variables and methods
    let that = this, private = {}

    if (this.initialize) {
        this.initialize(name, age)
    }
}
eva = new Person("Eva", 6)
R.print(eva.name)
R.print(eva.age)
