import qualified Tests.Ella.Request as Request
import qualified Tests.Ella.Response as Response
import qualified Tests.Ella.Framework as Framework
import qualified Tests.Ella.Processors.General as GeneralProcessors
import Test.HUnit

main = runTestTT (test [
                  , Request.tests
                  , Response.tests
                  , Framework.tests
                  , GeneralProcessors.tests
                  ])
