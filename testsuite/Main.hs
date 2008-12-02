import qualified Tests.Ella.Request as Request
import qualified Tests.Ella.Response as Response
import qualified Tests.Ella.Framework as Framework
import qualified Tests.Ella.Processors.General as GeneralProcessors
import qualified Tests.Ella.Forms.Widgets as Widgets
import Test.HUnit

main = runTestTT (test [ Request.tests
                       , Response.tests
                       , Framework.tests
                       , GeneralProcessors.tests
                       , Widgets.tests
                       ])
