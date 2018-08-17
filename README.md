# HTML to Reflex-DOM

A simple utility to convert HTML fragments to Reflex code.

To use please visit https://dfordivam.github.io/htmltoreflexdom/

Output from example.html (original html below)

```
  let
    attr1 = ("action" =: "process.php") <> ("id" =: "app-login")
  elAttr "form" attr1 $ do
    el "fieldset" $ do
      elClass "legend" "lclass" $ do
        text "login details"
        return ()
      el "div" $ do
        let
          attr2 = ("for" =: "user-name")
        elAttr "label" attr2 $ do
          text "username:"
          return ()
        let
          tiConf3 = def
                    & (textInputConfig_attributes .~ (constDyn (attr4)))
                    & (textInputConfig_inputType .~ "email")
                    & (textInputConfig_initialValue .~ "your username is your email address")
          attr4 = ("autofocus" =: "") <> ("name" =: "user-name") <>
                  ("required" =: "")
        ti3 <- textInput tiConf3
        return ()
      el "div" $ do
        let
          attr5 = ("for" =: "password")
        elAttr "label" attr5 $ do
          text "password:"
          return ()
        let
          tiConf6 = def
                    & (textInputConfig_attributes .~ (constDyn (attr7)))
                    & (textInputConfig_inputType .~ "password")
                    & (textInputConfig_initialValue .~ "6 digits, a combination of numbers and letters")
          attr7 = ("name" =: "password") <> ("required" =: "")
        ti6 <- textInput tiConf6
        return ()
      divClass "divc1" $ do
        let
          tiConf8 = def
                    & (textInputConfig_attributes .~ (constDyn (attr9)))
                    & (textInputConfig_inputType .~ "submit")
          attr9 = ("name" =: "login") <> ("value" =: "login")
        ti8 <- textInput tiConf8
        return ()
      return ()
    return ()
  el "fieldset" $ do
    el "legend" $ do
      text "choose some monster features"
      return ()
    el "div" $ do
      let
        cbConf10 = def
                   & (checkboxConfig_attributes .~ (constDyn (attr11)))
        attr11 = ("id" =: "scales") <> ("name" =: "feature") <>
                 ("value" =: "scales")
      cb10 <- checkbox True cbConf10
      let
        attr12 = ("for" =: "scales")
      elAttr "label" attr12 $ do
        text "scales"
        return ()
      return ()
    el "div" $ do
      let
        cbConf13 = def
                   & (checkboxConfig_attributes .~ (constDyn (attr14)))
        attr14 = ("id" =: "horns") <> ("name" =: "feature") <>
                 ("value" =: "horns")
      cb13 <- checkbox False cbConf13
      let
        attr15 = ("for" =: "horns")
      elAttr "label" attr15 $ do
        text "horns"
        return ()
      return ()
    el "div" $ do
      let
        cbConf16 = def
                   & (checkboxConfig_attributes .~ (constDyn (attr17)))
        attr17 = ("id" =: "claws") <> ("name" =: "feature") <>
                 ("value" =: "claws")
      cb16 <- checkbox False cbConf16
      let
        attr18 = ("for" =: "claws")
      elAttr "label" attr18 $ do
        text "claws"
        return ()
      return ()
    return ()
  el "fieldset" $ do
    el "legend" $ do
      text "audio settings"
      return ()
    let
      attr19 = ("for" =: "volume")
    elAttr "label" attr19 $ do
      text "volume"
      return ()
    let
      riConf20 = def
                 & (rangeInputConfig_attributes .~ (constDyn (attr21)))
      attr21 = ("id" =: "start") <> ("max" =: "11") <> ("min" =: "0") <>
               ("name" =: "volume")
    ri20 <- rangeInput riConf20
    let
      attr22 = ("for" =: "cowbell")
    elAttr "label" attr22 $ do
      text "cowbell"
      return ()
    let
      riConf23 = def
                 & (rangeInputConfig_attributes .~ (constDyn (attr24)))
                 & (rangeInputConfig_initialValue .~ 90.0)
      attr24 = ("id" =: "cowbell") <> ("max" =: "100") <> ("min" =: "0")
               <> ("name" =: "cowbell") <> ("step" =: "2")
    ri23 <- rangeInput riConf23
    return ()
  el "fieldset" $ do
    el "legend" $ do
      text "profile"
      return ()
    el "div" $ do
      let
        attr25 = ("for" =: "name")
      elAttr "label" attr25 $ do
        text "display name:"
        return ()
      let
        tiConf26 = def
                   & (textInputConfig_attributes .~ (constDyn (attr27)))
        attr27 = ("id" =: "name") <> ("name" =: "name")
      ti26 <- textInput tiConf26
      return ()
    el "div" $ do
      let
        attr28 = ("for" =: "avatar")
      elAttr "label" attr28 $ do
        text "profile picture:"
        return ()
      let
        fiConf29 = FileInputConfig ((constDyn (attr30)))
        attr30 = ("accept" =: "image/png, image/jpeg") <>
                 ("id" =: "avatar") <> ("name" =: "avatar")
      fi29 <- fileInput fiConf29
      return ()
    return ()
  let
    attr31 = ("for" =: "pet-select")
  elAttr "label" attr31 $ do
    text "choose a pet:"
    return ()
  let
    ddConf32 = def
               & (dropdownConfig_attributes .~ (constDyn (attr33)))
    attr33 = ("id" =: "pet-select")
    optMap34 = ("" =: "--please choose an option--") <>
               ("cat" =: "cat") <> ("dog" =: "dog") <> ("goldfish" =: "goldfish")
               <> ("hamster" =: "hamster") <> ("parrot" =: "parrot") <>
               ("spider" =: "spider")
  dd32 <- dropdown "hamster" (constDyn (optMap34)) ddConf32
  el "footer" $ do
    text "photo by ricky kharawala"
    el "br" $ return ()
    text "on unsplash"
    return ()
  el "p" $ do
    let
      attr35 = Map.empty
    (btnEl36,_) <- elAttr' "button" attr35 $ do
      text "default button"
      return ()
    {-
      let
        btnEv36 = domEvent Click btnEl36
    -}
    return ()
  el "p" $ do
    let
      attr37 = ("disabled" =: "")
    (btnEl38,_) <- elAttr' "button" attr37 $ do
      text "disabled button"
      return ()
    {-
      let
        btnEv38 = domEvent Click btnEl38
    -}
    return ()
  el "p" $ do
    let
      attr39 = ("name" =: "submit") <> ("type" =: "submit") <>
               ("value" =: "submit-true")
    (btnEl40,_) <- elAttr' "button" attr39 $ do
      text "form submit button"
      return ()
    {-
      let
        btnEv40 = domEvent Click btnEl40
    -}
    return ()
  el "p" $ do
    let
      attr41 = ("accesskey" =: "a")
    (btnEl42,_) <- elAttr' "button" attr41 $ do
      text "button with"
      el "u" $ do
        text "a"
        return ()
      text "ccesskey"
      return ()
    {-
      let
        btnEv42 = domEvent Click btnEl42
    -}
    return ()
  el "p" $ do
    let
      attr43 = ("class" =: "styled")
    (btnEl44,_) <- elAttr' "button" attr43 $ do
      text "fancy styled button"
      return ()
    {-
      let
        btnEv44 = domEvent Click btnEl44
    -}
    return ()
  el "fieldset" $ do
    el "legend" $ do
      text "input button"
      return ()
    el "form" $ do
      el "p" $ do
        text "default button:"
        let
          attr45 = ("class" =: "btn")
        (btnEl46,_) <- elAttr' "button" attr45 $ do
          text "click me"
          return ()
        {-
          let
            btnEv46 = domEvent Click btnEl46
        -}
        return ()
      el "p" $ do
        text "disabled button:"
        let
          attr47 = ("class" =: "btn") <> ("disabled" =: "")
        (btnEl48,_) <- elAttr' "button" attr47 $ do
          text "i am disabled"
          return ()
        {-
          let
            btnEv48 = domEvent Click btnEl48
        -}
        return ()
      el "p" $ do
        text "button with"
        el "u" $ do
          el "b" $ do
            text "a"
            return ()
          return ()
        text "ccesskey:"
        let
          attr49 = ("accesskey" =: "a") <> ("class" =: "btn")
        (btnEl50,_) <- elAttr' "button" attr49 $ do
          text "accesskey enabled"
          return ()
        {-
          let
            btnEv50 = domEvent Click btnEl50
        -}
        return ()
      el "p" $ do
        text "button without style:"
        let
          attr51 = Map.empty
        (btnEl52,_) <- elAttr' "button" attr51 $ do
          text "i'm a basic button"
          return ()
        {-
          let
            btnEv52 = domEvent Click btnEl52
        -}
        return ()
      return ()
    return ()
```
```
<form id="app-login" action="process.php">
    <fieldset>
        <legend class="lclass">login details</legend>
        <div>
            <label for="user-name">username:</label>
            <input name="user-name" type="email" placeholder="your username is your email address" required autofocus>

        </div>
        <div>
            <label for="password">password:</label>
            <input name="password" type="password" placeholder="6 digits, a combination of numbers and letters" required>
        </div>
        <div class="divc1">
            <input name="login" type="submit" value="login">
        </div>
    </fieldset>
</form>
<fieldset>
    <legend>choose some monster features</legend>

    <div>
        <input type="checkbox" id="scales" name="feature"
               value="scales" checked />
        <label for="scales">scales</label>
    </div>

    <div>
        <input type="checkbox" id="horns" name="feature"
               value="horns" />
        <label for="horns">horns</label>
    </div>

    <div>
        <input type="checkbox" id="claws" name="feature"
               value="claws" />
        <label for="claws">claws</label>
    </div>

</fieldset>
<fieldset>
    <legend>audio settings</legend>

    <label for="volume">volume</label>
    <input type="range" id="start" name="volume"
           min="0" max="11" />

    <label for="cowbell">cowbell</label>
    <input type="range" id="cowbell" name="cowbell" 
           min="0" max="100" value="90" step="2" />

</fieldset>
<fieldset>
    <legend>profile</legend>

    <div>
        <label for="name">display name:</label>
        <input type="text" id="name" name="name"/>
    </div>

    <div>
        <label for="avatar">profile picture:</label>
        <input type="file"
               id="avatar" name="avatar"
               accept="image/png, image/jpeg" />
    </div>

</fieldset>
<label for="pet-select">choose a pet:</label>

<select id="pet-select">
    <option value="">--please choose an option--</option>
    <option value="dog">dog</option>
    <option value="cat">cat</option>
    <option value="hamster" selected>hamster</option>
    <option value="parrot">parrot</option>
    <option value="spider">spider</option>
    <option value="goldfish">goldfish</option>
</select>

<footer>
    photo by ricky kharawala<br/> on unsplash
</footer>
<p><button>default button</button></p>

<p><button disabled>disabled button</button></p>

<p>
    <button name="submit" type="submit" value="submit-true">
        form submit button
    </button>
</p>

<p><button accesskey="a">button with <u>a</u>ccesskey</button></p>

<p><button class="styled">fancy styled button</button></p>

<fieldset>
    <legend>input button</legend>
    <form>
        <p> default button: <input type="button" class="btn" value="click me" /> </p>

        <p> disabled button: <input type="button" class="btn" value="i am disabled" disabled /> </p>

        <p>button with <u><b>a</b></u>ccesskey: <input type="button" class="btn" value="accesskey enabled" accesskey="a" /></p>

        <p>button without style: <input type="button" value="i'm a basic button" /></p>
    </form>
</fieldset>
```
