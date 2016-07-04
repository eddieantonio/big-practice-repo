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
/*global app*/
