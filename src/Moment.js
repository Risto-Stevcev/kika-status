var moment = require('moment')


exports.create = function(time) {
  return function() {
    return moment(time)
  }
}

exports.now = function() {
  return moment()
}

exports["diff'"] = function(start) {
  return function(end) {
    return function(period) {
      return start.diff(end, period)
    }
  }
}

exports["show'"] = function(time) {
  return time.format()
}
