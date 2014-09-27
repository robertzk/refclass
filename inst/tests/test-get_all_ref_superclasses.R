context('get_all_ref_superclasses')

test_that('it can get all reference superclasses for a simple example', {
  methods::setRefClass('test__c')
  methods::setRefClass('test__d', contains = 'test__c')
  expect_equal(get_all_ref_superclasses(list(getClass('test__d'))),
               c('test__c', 'envRefClass'))
  removeClass('test__c')
  removeClass('test__d')
})

