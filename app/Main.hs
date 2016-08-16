{-# LANGUAGE QuasiQuotes #-}

module Main where

  import Text.Hamlet
  import Text.Lucius
  import Text.Blaze.Html (preEscapedToHtml)
  import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
  import qualified Data.ByteString.Lazy as BSL
  import qualified System.Random as SR
  import qualified System.Random.Shuffle as SR
  import Data.Time.Clock.POSIX (getPOSIXTime)
  import Data.List.Split as DLS
  import qualified System.Directory as SD
  import Control.Monad
  import Control.Exception
  import Data.Monoid
  import qualified System.Process as SP
  import qualified Data.HashSet as S

  type TableList = [[String]]
  type HalfRowSize = Int
  type WordsList = [String]

  main :: IO ()
  -- main = mkRandomTable wordsList1 2 "words.html" "words.pdf"
  main = mkRandomTable syllables 5 "syllables.html" "syllables.pdf"

  mkRandomTable :: WordsList -> HalfRowSize -> FilePath -> FilePath -> IO ()
  mkRandomTable sourceList halfRowSize dumpHtmlFile dumpPdfFile = do
    let
      sourceListUni :: WordsList
      sourceListUni = let u = S.fromList sourceList in S.toList u

    sbs <- shuffledList sourceListUni

    let
      sl :: Int
      sl = length sbs

      fullRow :: Int
      fullRow = halfRowSize * 2

      list :: TableList
      list = DLS.chunksOf fullRow $ sbs ++ replicate (fullRow - sl `mod` fullRow) []

      html :: BSL.ByteString
      html = renderHtml $ content list halfRowSize

    rmFile dumpHtmlFile
    rmFile dumpPdfFile

    BSL.writeFile dumpHtmlFile html
    ignoreEx $ SP.callCommand ("wkhtmltopdf " <> dumpHtmlFile <> " " <> dumpPdfFile)


  rmFile :: FilePath -> IO ()
  rmFile fp = ignoreEx $ SD.removeFile fp

  ignoreEx :: IO () -> IO ()
  ignoreEx act = void (try act :: IO (Either SomeException ()))

  content :: TableList -> HalfRowSize -> Html
  content list halfRowSize = [shamlet|
  $doctype 5
  <html>
    <head>
      <meta charset=utf-8>
      <style>
        ^{css}
    <body>
      <table align=center style="padding-top:30px;">
        $forall ss <- list
          <tr>
            $with (ss1, ss2) <- splitAt halfRowSize ss
              $forall s1 <- ss1
                <td .syll>#{s1}
              <td width=50>
              $forall s2 <- ss2
                <td .syll>#{s2}
  |]

  css :: Html
  css = preEscapedToHtml $ renderCss $ [lucius|
  html *
  {
    font-family: "Lucida Console", Monaco, monospace;
    background-color: #F2F2F2;
  }
  .syll {
    font-size: 45px;
    font-weight: bolder;
    padding-right: 20px;
  }
  |] undefined

  shuffledList :: [String] -> IO [String]
  shuffledList list = do
    gen <- SR.mkStdGen <$> seed
    return $ SR.shuffle' list (length list) gen

  seed :: IO Int
  seed = round <$> getPOSIXTime

  wordsList1 :: WordsList
  wordsList1 = ["ЛАПКА", "ТОЛПА", "СТОПА", "ЛАМПА", "КНОПКА", "РУКА", "НОГА", "ЛИЦО",
    "ГОЛОВА", "НОС", "ГЛАЗА", "ПАЛЬЦЫ", "РОТ", "ВОЛОСЫ", "ПЯТКИ", "КОЛЕНИ", "ЩЕКИ",
    "КИСТЬ", "ГУБЫ", "ГРУДЬ", "СПИНА", "ШЕЯ", "ЖИВОТ", "ЛОБ", "ЗУБЫ", "БРОВИ", "ЯЗЫК",
    "БЕДРО", "ПЛЕЧО", "СТУПНЯ", "УХО", "ПУПОК", "ЗАТЫЛОК", "ЛОКОТЬ", "РЕСНИЦЫ", "ОЧКИ",
    "ПИЛА", "РАК", "СЫР", "ТОРТ", "ЧАСЫ", "ШАРИК", "ДЕНЬ", "ГЛАЗ", "СТОЛ", "НОЧЬ",
    "ГУСЬ", "ГРУЗ", "СТУК", "ТРЮК", "КРЮК", "ВРАЧ", "ГРОМ", "ПУСК", "ДЕД", "ДОМ",
    "КАША", "МАМА", "МЯЧ", "ВОДА", "КУБИК", "КУКЛА", "РУЧКА", "СУМКА", "МАЛЫШ",
    "ПОЕЗД", "ВАГОН", "ТУМАН", "РЕДИС", "РУЖЬЁ", "КАТОК", "МИСКА", "ВОЙНА", "МОТОР"]

  syllables :: WordsList
  syllables = ["ЛУ","ЛО","ЛА","ЛЭ","ЛЫ","МУ","МО","МА","МЭ","МЫ","НУ","НО","НА",
    "НЭ","НЫ","РУ","РО","РА","РЭ","РЫ","ВУ","ВО","ВА","ВЭ","ВЫ","ФУ","ФО","ФА",
    "ФЭ","ФЫ","ЗУ","ЗО","ЗА","ЗЭ","ЗЫ","СУ","СО","СА","СЭ","СЫ","БУ","БО","БА",
    "БЭ","БЫ","ПУ","ПО","ПЫ","ПЭ","ПЫ","ДУ","ДО","ДА","ДЭ","ДЫ","ТУ","ТО","ТА",
    "ТЭ","ТЫ","ГУ","ГО","ГА","ГЭ","ГЫ","КУ","КО","КА","КЭ","КЫ","ХУ","ХО","ХА",
    "ХЭ","ХЫ","ЖУ","ЖЁ","ЖО","ЖА","ЖЭ","ЖИ","ШУ","ШЁ","ШО","ША","ШЕ","ШИ","ЦУ",
    "ЦО","ЦА","ЦЕ","ЦИ","ЦЫ","ЧУ","ЧЁ","ЧО","ЧА","ЧЕ","ЧИ","ЩУ","ЩЁ","ЩО","ЩА",
    "ЩЕ","ЩИ","ЛЮ","ЛЁ","ЛЯ","ЛЕ","ЛИ","МЮ","МЁ","МЯ","МЕ","МИ","НЮ","НЁ","НЯ",
    "НЕ","НИ","РЮ","РЁ","РЯ","РЕ","РИ","ВЮ","ВЁ","ВЯ","ВЕ","ВИ","ФЮ","ФЁ","ФЯ",
    "ФЕ","ФИ","ЗЮ","ЗЁ","ЗЯ","ЗЕ","ЗИ","СЮ","СЁ","СЯ","СЕ","СИ","БЮ","БЁ","БЯ",
    "БЕ","БИ","ПЮ","ПЁ","ПЯ","ПЕ","ПИ","ДЮ","ДЁ","ДЯ","ДЕ","ДИ","ТЮ","ТЁ","ТЯ",
    "ТЕ","ТИ","ГЮ","ГЁ","ГЯ","ГЕ","ГИ","КЮ","КЁ","КЯ","КЕ","КИ","ХЮ","ХЁ","ХЯ",
    "ХЕ","ХИ","НЪ","РЪ","ВЪ","ЗЪ","СЪ","БЪ","ДЪ","ТЪ","ХЪ","ЖЪ","ЖЬ","ШЬ","ЧЬ",
    "ШЬ","ЛЬ","МЬ","НЬ","РЬ","ВЬ","ФЬ","ЗЬ","СЬ","БЬ","ПЬ","ДЬ","ТЬ","ГЬ","КЬ",
    "ХЬ","ЙЮ","ЙО","ЙА","ЙЯ","ЙЕ","ЙИ","У","О","А","Э","Ы","Ю","Ё","Я","Е","И",
    "Л","М","Н","Р","В","Ф","З","С","Б","П","Д","Т","Г","К","Х","Ж","Ш","Ц","Ч",
    "Щ","Й"]
