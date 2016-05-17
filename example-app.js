var app = angular.module('exampleApp', ['ngRoute']);

app.config(function ($routeProvider) {
  $routeProvider
    .when('/', {
      controller: 'CustomerController',
      templateUrl: 'views/index.html'
    })
    .when('/partial2', {
      controller: 'CustomerController',
      templateUrl: 'views/alternate.html'
    })
    .otherwise({ redirectTo: '/' });
});

app.controller('CustomerController', function ($scope) {
  $scope.customers = [
    { name: 'Dave', city: 'Phoenix'},
    { name: 'Napur', city: 'Denver' },
    { name: 'Heedy', city: 'Dallas'}
  ];

  $scope.addCustomer = function() {
    $scope.customers.push({
      name: $scope.newCustomer.name,
      city: $scope.newCustomer.city
    });
  };
});
