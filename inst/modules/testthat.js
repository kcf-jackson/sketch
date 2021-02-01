test = function() {
    // public variables and methods
    let self = this
    self.total = 0
    self.pass = 0
    self.error_msg = Array()
    self.reset = function() {
        self.total = 0
        self.pass = 0
    }
    self.conduct_test = function() {
        self.total = self.total + 1
    }
    self.pass_test = function() {
        self.pass = self.pass + 1
    }
    self.report = function() {
        return({ "total": self.total, "pass": self.pass, "fail": self.total - self.pass, "error_msg": self.error_msg })
    }
    self.expect_true = function(object) {
        let msg
        self.conduct_test()
        if (object != true) {
            msg = `Error: ${object} isn't true.`
            self.error_msg.push(msg)
            try {
                throw new Error(msg)
            } catch(error) {
                console.log(error)
            }
        } else {
            self.pass_test()
        }
    }
    self.expect_false = function(object) {
        let msg
        self.conduct_test()
        if (object != false) {
            msg = `Error: ${object} isn't false.`
            self.error_msg.push(msg)
            try {
                throw new Error(msg)
            } catch(error) {
                console.log(error)
            }
        } else {
            self.pass_test()
        }
    }
    self.expect_equal = function(object, expected) {
        let msg
        self.conduct_test()
        if (object != expected) {
            msg = `Error: ${object} not equal to ${expected}.`
            self.error_msg.push(msg)
            try {
                throw new Error(msg)
            } catch(error) {
                console.log(error)
            }
        } else {
            self.pass_test()
        }
    }
    // private variables and methods
    let private = {}
    
    if (self.initialize) {
        self.initialize()
    }
}
testthat = new test()
