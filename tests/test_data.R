context("output")

test_that("big count data complete-ish",
          {
            #All labels
            expect_true(all(complete.cases(df.iso_big_count$association)))
            expect_true(all(complete.cases(df.iso_big_count$iso_country_name)))
            expect_true(all(complete.cases(df.iso_big_count$iso_alpha_3_code)))
            
            #All player data
            expect_true(all(complete.cases(df.iso_big_count$all_players)))
            expect_true(all(complete.cases(df.iso_big_count$registered_players)))
            expect_true(all(complete.cases(df.iso_big_count$unregistered_players)))
          })

test_that("confederation data complete",
          {
            expect_true(all(complete.cases(df.confederations)))
          })

test_that("associations included in big count are a subset of all associations",
          {
            expect_equal(df.confederations %>% 
                           full_join(df.iso_big_count,
                                     by = c("big_count_association" = "association")) %>% 
                           nrow,
                         nrow(df.confederations))
            
            expect_equal(df.iso_big_count %>% 
                           inner_join(df.confederations,
                                      by = c('association' = 'big_count_association')) %>% 
                           nrow,
                         df.iso_big_count %>% 
                           nrow)
          })