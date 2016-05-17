var app = angular.module('exampleApp', ['ngRoute']);

app.config(function ($routeProvider) {
  $routeProvider
    .when('/', {
      controller: 'CustomerController',
      templateUrl: 'app/views/index.html'
    })
    .when('/alt', {
      controller: 'CustomerController',
      templateUrl: 'app/views/alternate.html'
    })
    .otherwise({ redirectTo: '/' });
});

app.factory('simpleFactory', function () {
  var customers = [
    { name: 'Dave', city: 'Phoenix'},
    { name: 'Napur', city: 'Denver' },
    { name: 'Heedy', city: 'Dallas'}
  ];

  return {
    getCustomers() {
      return customers;
    }
  };
});

app.controller('CustomerController', function ($scope, simpleFactory) {
  $scope.customers = [];

  init();

  function init() {
    $scope.customers = simpleFactory.getCustomers();
  }

  $scope.addCustomer = function() {
    $scope.customers.push({
      name: $scope.newCustomer.name,
      city: $scope.newCustomer.city
    });
  };
});
