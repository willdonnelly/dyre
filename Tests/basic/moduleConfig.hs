import BasicTest
import qualified MyConfig
main = basicTest $ defaultConfig { message = MyConfig.message }
