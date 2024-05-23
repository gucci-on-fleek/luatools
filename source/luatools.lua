--=-----------------------------
--= \section{Initialization} ---
--=-----------------------------
--=
--= Before we can do anything else, we need to run some various initialization
--= steps.

--= \subsection{Loading}
--=
--= Make sure we haven't already been loaded.

if luatools then
    return luatools
end


--= We require \typ{luaotfload} (for \typ{lualibs} and \typ{luatexbase}) when
--= we're using Plain~\LuaTeX{}, so we'll load it here if it's not already
--= loaded.
if not package.loaded.luaotfload then
    tex.runtoks(function()
        tex.sprint[[\input luaotfload.sty]]
    end)
end


--= \subsection{Local Functions}
--=
--= We're going to use some of the \ConTeXt{} \typ{lualibs} functions quite a
--= bit, so we'll save some (private) local versions of them here.

--- @diagnostic disable - The typechecker hasn't heard of these functions, so we
---                       disable it for now.

--= Sets the \typ{__index} metamethod of a table.
--- @overload fun(t: table, f: function|table): table
--- @overload fun(f: function): table
local setmetatableindex = table.setmetatableindex

--= Does \lua{{ "a", "b", "c" }} \to \lua{{ a = 1, b = 2, c = 3 }}
--- @generic K,V - -
--- @type fun(t: table<K, V>): table<V, K>
local table_swapped = table.swapped

--= Inserts a value into a table.
local insert = table.insert

--= Appends a table to another table, in place.
--- @type fun(a: table, b: table): table
local append = table.append

--= Concatenates a table into a string.
local concat = table.concat

--= Gets the character at a given codepoint.
local utf_char = utf8.char

--= Splits a string into lines.
--- @type fun(s: string): table
string.splitlines = string.splitlines

--- @diagnostic enable - -


--= \subsection{\typ{luatools}}
--=
--= Now, we define a global table \typ{luatools} to hold all of our defined
--= functions.

--- @class luatools -               The module class.
--- @field config   luatools.config The module-local data.
luatools = {}

--= Before we can do anything else, we need to figure out what format and engine
--= we are running in. This is done by checking \typ{tex.formatname} and
--= \typ{status.luatex_engine}.

local fmt_extras = {}

-- Get the format
local tex_format = tex.formatname or "texlua"

local format
if tex_format:find("cont") then
    format = "context"
elseif tex_format:find("latex") then
    format = "latex"
    fmt_extras.luatexbase = true
elseif tex_format == "luatex" or
       tex_format == "luahbtex" or
       tex_format:find("plain")
then
    format = "plain"
    fmt_extras.luatexbase = true
elseif tex_format:find("optex") then
    format = "optex"
elseif tex_format == "texlua" then
    format = "texlua"
else
    format = "unknown"
end

-- Get the engine
local tex_engine = status.luatex_engine

local engine
if tex_engine == "luatex" then
    engine = "luatex"
elseif tex_engine == "luahbtex" then
    engine = "luahbtex"
    fmt_extras.hb = true
elseif tex_engine == "luametatex" then
    engine = "luametatex"
    fmt_extras.lmtx = true
else
    engine = "unknown"
end

--= \subsection{\typ{luatools.fmt}}
--=
--= \typ{luatools.fmt} holds the current format and engine. There are four
--= different ways to use it:
--=
--= \startitemize[n]
--= \item By indexing it by the format or engine name:
--=      \startcode[lua]
--=          if lt.fmt.latex then
--=              -- LaTeX-specific code
--=          elseif lt.fmt.context then
--=              -- ConTeXt-specific code
--=          end
--=      \stopcode
--=
--= \item By calling it:
--=     \startcode[lua]
--=         if lt.fmt() == "latex" then ...
--=     \stopcode
--=
--= \item By converting it to a string:
--=     \startcode[lua]
--=         print("Current format: " .. lt.fmt)
--=         -- Prints "Current format: latex"
--=     \stopcode
--= \item By explicitly asking for the format or engine:
--=     \startcode[lua]
--=         if lt.fmt.engine == "hb" then ...
--=     \stopcode
--= \stopitemize
--=
--= \noindent The possible values for \typ{lt.fmt} are:
--= \startitemize[1]
--= \item Formats:
--=     \startitemize[2]
--=     \item \typ{plain}: Plain \TeX{}
--=     \item \typ{latex}: \LaTeX{}
--=     \item \typ{context}: \ConTeXt{}
--=     \item \typ{optex}: Op\TeX{}
--=     \item \typ{texlua}: \LuaTeX{} in Lua-only mode (aka \typ{texlua})
--=     \item \typ{unknown}: Anything else
--=     \stopitemize
--= \item Engines:
--=     \startitemize[2]
--=     \item \typ{luatex}: \LuaTeX{}
--=     \item \typ{luahbtex}, \typ{hb}: Lua\ac{HB}\TeX
--=     \item \typ{luametatex}, \typ{lmtx}: LuaMeta\TeX{}
--=     \item \typ{unknown}: Anything else
--=     \stopitemize
--= \stopitemize


luatools.fmt = setmetatable({
    format = format,
    engine = engine
    }, {
    __index = function(t, k)
        local v = (k == format) or (k == engine) or fmt_extras[k] or false
        t[k] = v
        return v
    end,
    __call = function(t)
        return format
    end,
    __tostring = function(t)
        return format
    end
})


-- Forward declare our private instance
local lt


--= \subsection{\typ{luatools.init}}
--=
--= You generally need to instantiate \typ{luatools} before you can use it:
--= \startcode[lua]
--=     require "luatools"
--=     local lt = luatools.init {
--=         name = "test",
--=     }
--= \stopcode

--- @class luatools.config   -   A table containing module-local data.
--- @field name         string   The name of the module.
--- @field ns?          string   The namespace of the module.
--- @field version?     string   The version of the module.
--- @field date?        string   The date of the module, in the format
---                              \typ{YYYY-MM-DD}.
--- @field description? string   A short description of the module.
--- @field debug?       boolean  Whether to show debug messages.
--- @field expl?        boolean  Whether to use \LaTeX{} expl3 conventions.
--- @field self?        luatools The module root.

--- @param  config   luatools.config A table containing module-local data.
--- @return luatools lt              A new instance of the module.
function luatools.init(config)
    -- Make sure that we have the required fields
    if not config.name then
        lt.msg:error("Module name is required.")
    end

    config.ns    = config.ns or config.name
    config.debug = config.debug or false
    config.expl  = config.expl or false

    -- Create a new table
    local self = { config = config }

    -- Make sure that there's always a pointer to the root table
    self.self = self
    config.self = self

    for k, v in pairs(luatools) do
        if type(v) == "table" then
            v.self = self
        end
        self[k] = v
    end

    -- Initialize by engine
    if self.fmt.luatexbase then
        local date
        if self.config.date then
            -- LaTeX expects slashed dates
            date = self.config.date:gsub("-", "/")
        end
        luatexbase.provides_module {
            name = self.config.name,
            date = date,
            version = self.config.version,
            description = self.config.description,
        }
    elseif self.fmt.context then
        modules = modules or {}
        modules[self.config.name] = {
            version = self.config.version,
            date    = self.config.date,
            comment = self.config.description,
        }
    else
        -- OpTeX doesn't have a module system, so we'll just print an info
        -- message.
        self.msg:info(
            "Loading version " ..
            (self.config.version or "unknown") ..
            " (" ..
            (self.config.date or "unknown") ..
            ")."
        )
    end

    return self
end


--= Initialize ourself.
--=
--= We need to use \typ{setmetatableindex} here because we \typ{luatools.init}
--= creates a copy of the \typ{luatools} table, but we're still adding new
--= functions to it after this.
lt = setmetatableindex(
        luatools.init {
        name        = "luatools",
        ns          = "lt",
        version     = "0.0.0", --%%version
        date        = "2021-07-01", --%%dashdate
        description = "Cross-format Lua helpers."
    },
    luatools
)


--- Ternary operator for LuaTeX and LuaMetaTeX.
--- @generic        T - -
--- @param   luatex T Value if running in \LuaTeX{}.
--- @param   lmtx   T Value if running in LuaMeta\TeX{}.
--- @return  T    out -
local function luatex_lmtx(luatex, lmtx)
    if lt.fmt.lmtx then
        return lmtx
    else
        return luatex
    end
end


--=-----------------------
--= \section{Messages} ---
--=-----------------------
--=
--= Here, we define some functions used for printing messages.

--- @class luatools.msg - A table containing message functions.
--- @field self luatools  The module root.
luatools.msg = {}


--= \subsection{\typ{luatools.msg:console}}
--=
--= Prints a message to only the console.

local write_nl = texio.write_nl or texio.writenl
local CONSOLE = luatex_lmtx("term", "terminal")

--- @param  ... string The messages to print.
--- @return nil -      -
function luatools.msg:console(...)
    local msg = concat({...}, "\t")
    write_nl(CONSOLE, msg)
end


--= \subsection{\typ{luatools.msg:log}}
--=
--= Prints a message to the log file.

local LOG_FILE = luatex_lmtx("log", "logfile")

--- @param  ... string The messages to print.
--- @return nil -      -
function luatools.msg:log(...)
    local msg = concat({...}, "\t")
    write_nl(LOG_FILE, msg)
end


--= \subsection{\typ{luatools.msg:print}}
--=
--= Prints a message to both the console and the log file.

local TERM_AND_LOG = luatex_lmtx("term and log", "terminal_and_logfile")

--- @param  ... any The messages to print.
--- @return nil -   -
function luatools.msg:print(...)
    -- Make sure that all messages are strings
    local msgs = {...}
    for i, msg in ipairs(msgs) do
        msgs[i] = tostring(msg)
    end

    local msg = concat(msgs, "\t")
    write_nl(TERM_AND_LOG, msg)
end


--= \subsection{\typ{luatools.msg:debug}}
--=
--= Prints a message to both the console and the log file, but only if debugging
--= is enabled.

--- @param  ... any The messages to print.
--- @return nil -   -
function luatools.msg:debug(...)
    self = self.self

    if self.config.debug then
        self.msg:print(self.config.name, ...)
    end
end


--= \subsection{\typ{luatools.msg:info}}
--=
--= Prints a message to the log file containing the module's name.

--- @param  msg string The message to print.
--- @return nil -      -
function luatools.msg:info(msg)
    self = self.self

    if self.fmt.context then
        -- We don't want the info messages on the terminal, but ConTeXt doesn't
        -- provide any log-only reporters, so we need this hack.
        local info = logs.reporter(self.config.name, "info")

        function luatools.msg:info(msg)
            logs.pushtarget("logfile")
            info(msg)
            logs.poptarget()
        end
    elseif self.fmt.luatexbase then
        -- For Plain and LaTeX, we can use the built-in info reporter.
        local name = self.config.name

        function luatools.msg:info(msg)
            luatexbase.module_info(name, msg)
        end
    else
        -- OpTeX doesn't have a special info reporter, so we need to do it
        -- manually.
        local start = self.config.name .. " Info: "
        function luatools.msg:info(msg)
            self = self.self

            self.msg:log(start .. msg)
        end
    end

    self.msg:info(msg)
end


--= \subsection{\typ{luatools.msg:warning}}
--=
--= Prints a warning message to the console and log file containing the module's
--= name.

--- @param  msg string The message to print.
--- @return nil -      -
function luatools.msg:warning(msg)
    self = self.self

    if self.fmt.context then
        -- Here, we can just use the built-in warning reporter.
        local warning = logs.reporter(self.config.name, "warning")

        function luatools.msg:warning(msg)
            warning(msg)
        end
    elseif self.fmt.luatexbase then
        local name = self.config.name

        function luatools.msg:warning(msg)
            luatexbase.module_warning(name, msg)
        end
    else
        local start = self.config.name .. " Warning: "
        function luatools.msg:warning(msg)
            self = self.self

            self.msg:print(start .. msg)
        end
    end

    self.msg:warning(msg)
end


--= \subsection{\typ{luatools.msg:error}}
--=
--= Prints an error message to the console and log file containing the module's
--= name and pauses compilation.

--- @param  msg string The message to print.
--- @return nil -      -
function luatools.msg:error(msg)
    self = self.self

    if self.fmt.luatexbase then
        local name = self.config.name

        function luatools.msg:error(msg)
            luatexbase.module_error(name, msg)
        end
    else
        -- For ConTeXt and OpTeX, just use a raw Lua error.
        local start = self.config.name .. " Error: "
        function luatools.msg:error(msg)
            error(start .. msg, 2)
        end
    end

    self.msg:error(msg)
end


--=------------------------
--= \section{Utilities} ---
--=------------------------
--=
--= Here, we define some general-purpose utility functions.

--- @class luatools.util - A table containing utility functions.
--- @field self luatools  The module root.
luatools.util = {}


--= \subsection{\typ{luatools.util:memoized}}
--=
--= Memoizes a function call/table index.

--- @param  func function<any, any> The function to memoize
--- @return table -                 The “function”
function luatools.util:memoized(func)
    return setmetatable({}, { __index = function(cache, key)
        local ret = func(key, cache)
        cache[key] = ret
        return ret
    end,  __call = function(self, arg)
        return self[arg]
    end })
end


--= \subsection{\typ{luatools.util:type}}
--=
--= A variant of \typ{type} that also works on userdata.

--- @param val any   The value to get the type of
--- @return string - The type of the value
function luatools.util:type(val)
    local meta = getmetatable(val)
    if meta and meta.__name then
        return meta.__name
    else
        return type(val)
    end
end


--= \subsection{\typ{scope}}
--=
--= The builtin \LuaTeX{} functions use multiple inconsistent ways to set the
--= desired scope for a function. In this module, we'll use a terminating
--= \typ{scope} parameter that can be set to either \lua{"local"} or \lua{nil}
--= for a local scope, or \lua{"global"} for a global scope.
--- @alias scope "local"|"global"|nil -


--=---------------------------------
--= \section{Tokens (Low-Level)} ---
--=---------------------------------
--=
--= The \typ{luatools.token} table is the low-level interface for working with
--= \TeX{} tokens. It is a very thin wrapper around the \LuaTeX{} \typ{token}
--= functions; for a more user-friendly interface, see the high-level
--= \typ{luatools.macro} submodule.

--= \subsection{Token Types}
--=
--= Representing tokens in \LuaTeX{} is a bit of a mess since there are 3
--= different Lua “types” that a token can have, and most of the functions only
--= work on one of them.

--= First off, we have the userdata \typ{token} objects, which are returned by
--= the builtin \typ{token.create} function.
--- @alias user_tok luatex.token -
--- @class luatex.token -  \LuaTeX{} token “userdata” objects.
--- @field command integer The command code (catcode) of the token.
--- @field mode    integer The mode (charcode) of the token.
--- @field tok     integer The token number (an index into \typ{eqtb}).

--= Next, given a csname as a string, we can easily get the underlying token
--= object.
--- @alias csname_tok string -

--= Finally, a token can be represented as the the table \lua{{cmd, chr, cs}},
--= where \typ{cmd} is the command code, \typ{chr} is the character code, and
--= \typ{cs} is an index into the \TeX{} hash table.
--- @class (exact) tab_tok A table representing a token.
--- @field [1] integer     (cmd) The command code of the token.
--- @field [2] integer     (chr) The character code of the token.
--- @field [3] integer?    (cs)  The index into the \TeX{} hash table.

--- @alias any_tok user_tok|csname_tok|tab_tok - Any type of token.


--= \subsection{Token List Types}
--=
--= Single tokens are rarely useful, so we'll generally be working with “token
--= lists” instead. Once again, there are multiple incompatible ways to
--= represent these.

--= The most basic way to represent a token list is as a table of tokens. In
--= this format, each of the entries can either be a \typ{user_tok} or a
--= \typ{tab_tok}.
--- @alias tab_toklist (tab_tok | user_tok)[] -

--= The other way to represent a token list is as a string that is tokenized by
--= TeX in a function-dependent manner. These strings are generally processed
--= either using the current catcode regime or using the “\tex{meaning}”
--= catcodes.
--- @alias str_toklist string -

--- @alias any_toklist tab_toklist|str_toklist - Any type of token list.


--= \subsection{\typ{luatools.token}}
--=
--= We define a table \typ{luatools.token} to hold all of our token functions.
--- @class luatools.token -            A table containing token functions.
--- @field self luatools               The module root.
--- @field [string] function<any, any> A wrapper around the \LuaTeX{}
---                                    \typ{token} functions.
luatools.token = setmetatableindex(function(t, k)
--= LuaMeta\TeX{} removes underscores from most of its \typ{token} functions, so
--= we'll try removing any underscores if we can't find the function.
    local v = token[k] or token[k:gsub("_", "")]
    t[k] = v
    return v
end)


--= \subsection{\typ{luatools.token:run_toklist}}
--= Runs the given function or token list register inside a new “local \TeX{}
--= run”.
--- @type fun(func: function<nil, nil>, local: boolean?, grouped: boolean?): nil
luatools.token.run_toklist = luatex_lmtx(tex.runtoks, tex.runlocal)


--= \subsection{\typ{luatools.token:run}}
--=
--= Runs a piece of \TeX{} code.

--- @param  code any_toklist The code to run.
--- @return nil  - -
function luatools.token:run(code)
    self = self.self

    local run
    if type(code) == "string" then
        run = code:splitlines()
    else
        run = code
    end

    self.token.run_toklist(function()
        tex.tprint(run)
    end, true)
end


--= \subsection{\typ{luatools.token.cmd}}
--=
--= Gets the internal “command code”, indexed by the command name.
--- @type table<string, integer> - -
luatools.token.cmd = table_swapped(token.commands())


--= \subsection{\typ{luatools.token.cached}}
--=
--= Gets a \TeX{} token by name and caches it for later.

--- @overload fun(name: csname_tok): user_tok
--- @overload fun(chr: integer, cmd: integer): user_tok
local token_create = token.create

--- @type fun(name: csname_tok): boolean
local token_defined = luatex_lmtx(token.is_defined, token.isdefined)

--- @type table<string, user_tok> - -
luatools.token.cached = lt.util:memoized(function(csname)
    -- We don't want to cache undefined tokens
    if token_defined(csname) then
        return token_create(csname)
    end
end)

--= Pre-define some special tokens
luatools.token.cached["{"] = token_create(0, lt.token.cmd.left_brace)
luatools.token.cached["}"] = token_create(0, lt.token.cmd.right_brace)


--= \subsection{\typ{luatools.token:set_csname}}
--=
--= Sets the given csname to be equal to the given token.
--=
--= There's surprisingly no way to define a csname with arbitrary tokens from
--= Lua, so we instead have to use the \tex{let} primitive from \TeX{}.

--- @param  csname  string  The name of the csname.
--- @param  tok     any_tok The token to set the csname to.
--- @param  scope   scope   -
--- @return nil     -       -
function luatools.token:set_csname(csname, tok, scope)
    self = self.self

    -- We need to define a token with the provided csname first, otherwise we
    -- get an `undefined_cs`-type token, which can't be passed to TeX without
    -- throwing an error.
    self.token.set_char(csname, 0)

    -- We can't mix strings and tokens together in a token list, so if we're
    -- given a `csname_tok`, we need to convert it to a `user_tok` first.
    if type(tok) == "string" then
        tok = token_create(tok)
    end

    local toks = {
        self.token.cached["let"],
        token_create(csname),
        tok
    }

    if scope == "global" then
        insert(toks, 1, self.token.cached["global"])
    end

    self.token:run(toks)
end


--=--------------------------------
--= \section{\TeX{} Interfaces} ---
--=--------------------------------
--=
--= The \typ{luatools.tex} table is the high-level interface for working with
--= \TeX{} tokens and registers.

--- @class luatools.tex - A table containing \TeX{} functions.
--- @field self luatools  The module root.
luatools.tex = {}


--= \subsection{\typ{luatools.tex:mangle_name}}
--=
--= Every \TeX{} format uses different naming conventions for their user-defined
--= macros and registers, which makes it quite tricky to write cross-format
--= code. This function converts a friendly “Lua” name into a format-specific
--= “\TeX{}” name.

--= Right now, we'll only support \TeX{} registers with the following types:
--- @alias register_type "dimen"|"count"

--= For consistency, we're using the standard \TeX{} names for the types, so
--= we'll need to convert them to the expl3 names if we're using expl3.
--- @type table<register_type, string> - -
local expl_types = {
    dimen = "dim",
    count = "int",
}

--= Macros aren't registers, but they still have a csname, so we'll support them
--= in \lua{luatools.tex} too.
--- @alias tex_type register_type|"macro"

--- @class _name_params -       The parameters for the mangled name.
--- @field name        string   The name to mangle.
--- @field type        tex_type The \TeX{} type that the name points at.
--- @field arguments?  string   An expl3 macro “signature” (the characters after
---                             \typ{:} in the macro name). (Default: \lua{""})
--- @field scope?      scope    Global or local? (Default: \lua{"local"})
--- @field visibility? "public"|"private" Public or private? (Default:
---                                       \lua{"private"})

--- @param  params _name_params The parameters for the mangled name.
--- @return string -            The mangled name.
function luatools.tex:mangle_name(params)
    self = self.self

    -- Set the defaults
    local name = params.name
    params.arguments  = params.arguments  or ""
    params.scope      = params.scope      or "local"
    params.visibility = params.visibility or "private"

    -- Add the namespace prefix
    name = self.config.ns .. "_" .. name

    -- Convert the word separators to the appropriate character
    if self.config.expl then
        -- expl3 uses underscores for public and private variables, so there's
        -- nothing to do here.
    elseif params.visibility == "public" then
        -- Public variables have no separators since only letters are allowed
        -- in csnames when using normal catcodes.
        name = name:gsub("_", "")
    elseif self.fmt.plain or self.fmt.latex then
        -- Plain and LaTeX use `@` as a separator for private variables.
        name = name:gsub("_", "@")
    else
        -- ConTeXt and OpTeX use underscores for private variables, so there's
        -- also nothing to do here.
    end

    -- Handle type, scope, and visibility prefixes and suffixes
    if self.config.expl then
        if params.type == "macro" then
            -- Visibility prefixes:
            if params.visibility == "private" then
                name = "__" .. name
            end

            -- Argument suffixes:
            name = name .. ":" .. params.arguments
        else
            -- Visibility prefixes:
            if params.visibility == "private" then
                name = "_" .. name
            end

            -- Scope prefixes:
            if params.scope == "global" then
                name = "g_" .. name
            else
                name = "l_" .. name
            end

            -- Type suffixes:
            name = name .. "_" .. expl_types[params.type]
        end
    elseif self.fmt.optex and params.visibility == "private" then
        -- Visibility prefixes:
        name = "_" .. name
    else
        -- Only expl3 puts the type and scope of a variable in its name, so for
        -- all other formats, there's nothing to do here.
    end

    return name
end


--= \subsection{\typ{luatools.tex:allocate}}
--=
--= Creates a new \TeX{} register with the given parameters.

--= We'll cache the full “\TeX{} name” of any csname that we look up so that we
--= can avoid looking it up again in the future.
--- @type table<string, csname_tok>
local name_cache = {}

--- @param  params _name_params The parameters for the register.
--- @return nil    -            -
function luatools.tex:allocate(params)
    self = self.self

    if params.type == "macro" then
        lt.msg:error("Use ``lt.macro:define'' to define new macros.")
        return --- @diagnostic disable-line: missing-return-value
    end

    local name = self.tex:mangle_name(params)

    -- We don't want to allocate the same register twice
    if token_defined(name) or name_cache[params.name] then
        lt.msg:warning("Register " .. name .. " already allocated.")
    end

    self.token:run {
        self.token.cached["new" .. params.type],
        token_create(name)
    }

    name_cache[params.name] = name
end


--=---------------------
--= \section{Macros} ---
--=---------------------
--=
--= The \typ{luatools.macro} table contains functions for working with \TeX{}
--= macros (tokens defined by \tex{def}).

--- @class luatools.macro - A table containing macro functions.
--- @field self luatools    The module root.
luatools.macro = {}


--= \subsection{\typ{luatools.macro:unexpanded}}
--=
--= Gets the raw, unexpanded “replacement text” of a macro.

--- @param  name string The name of the macro.
--- @return string -    The raw replacement text.
function luatools.macro:unexpanded(name)
    self = self.self

    return self.token.get_macro(name)
end


--= \subsection{\typ{luatools.macro:expanded}}
--=
--= Gets the expanded “replacement text” of a macro, like \tex{message} and
--= \tex{expanded}.

--- @param  name string The name of the macro.
--- @return string -    The expanded replacement text.
function luatools.macro:expanded(name)
    --- @diagnostic disable-next-line TODO
end


--= \subsection{\typ{luatools.macro:super_expanded}}
--=
--= Fully-expands a macro by typesetting it in a box and reading the box's
--= contents.

--- @param  name string The name of the macro.
--- @return string -    The fully-expanded replacement text.
function luatools.macro:super_expanded(name)
    --- @diagnostic disable-next-line TODO
end


--= \subsection{\typ{luatools.macro:define}}
--=
--= Defines a new macro.
function luatools.macro:define(...)
    --- @diagnostic disable-next-line TODO
end


--=--------------------
--= \section{Nodes} ---
--=--------------------
--=
--= Here, we define some functions for working with \LuaTeX{} nodes.

--- @class luatex.node -  \LuaTeX{} node “userdata” objects.
--- @field id      number The node's type.
--- @field subtype number The node's subtype.
--- @field next?   node   The next node in the list.
--- @field prev?   node   The previous node in the list.
--- @alias node    luatex.node

--- @class luatools.node -       A table containing node functions.
--- @field self luatools         The module root.
--- @field [string] fun(...):... A wrapper around the \LuaTeX{} \typ{node}
---                              functions.
luatools.node = setmetatableindex(function(t, k)
--= LuaMeta\TeX{} removes underscores from most of its \typ{node} functions, so
--= we'll try removing any underscores if we can't find the function.
    local v = node[k] or node[k:gsub("_", "")]
    t[k] = v
    return v
end)


--= \subsection{\typ{luatools.node:type}}
--=
--= Gets the type and subtype of a node as pair of strings.

local node_id_to_type = node.types()
local node_subid_to_subtype = {}

for id, name in pairs(node_id_to_type) do
    node_subid_to_subtype[id] = node.subtypes(name)
end

--= LuaMeta\TeX{} gives different names to some node types, and these names are
--= generally much more sensible, so we'll use the same names for \LuaTeX{} too.
if not lt.fmt.lmtx then
    local node_type_to_id = table_swapped(node_id_to_type)

    -- Inserts
    local insert_id = node_type_to_id["ins"]
    node_id_to_type[insert_id] = "insert"

    -- Paragraphs
    local paragraph_id = node_type_to_id["local_par"]
    node_id_to_type[paragraph_id] = "par"

    -- Some other names are renamed too, but they're rarely used so we'll ignore
    -- them.
end


--- @param  n node           The node to get the type of.
--- @return string,string? - The type and subtype of the node.
function luatools.node:type(n)
    local type = node_id_to_type[n.id]
    local subtypes = node_subid_to_subtype[n.id]

    local subtype
    if subtypes and n.subtype then
        subtype = subtypes[n.subtype]
    end

    return type, subtype
end


--= \subsection{\typ{luatools.node:is}}
--=
--= Checks if a node is the specified type or subtype.

--- @param  n     node   The node to check.
--- @param  query string The type or subtype to check against.
--- @return boolean -    Whether the node is of the specified type or subtype.
function luatools.node:is(n, query)
    self = self.self

    local type, subtype = self.node:type(n)

    return query == type or query == subtype
end


--= \subsection{\typ{luatools.node:new}}
--=
--= Creates a new node with the specified parameters.

--- @class _node_new -     The node creation parameters.
--- @field type     string The type of the new node.
--- @field subtype? string The subtype of the new node.
--- @field [string] any    Any additional fields to set on the new node.

--- @param  params _node_new The parameters for the new node.
--- @return node   -         The new node.
function luatools.node:new(params)
    -- Create the new node
    local n = node.new(params.type, params.subtype)

    -- Set any additional fields
    params.type = nil
    params.subtype = nil
    for k, v in pairs(params) do
        n[k] = v
    end

    return n
end


--= \subsection{\typ{luatools.node:join}}
--=
--= Joins a list of nodes into a node list.

--- @param  nodes node[] The nodes to join.
--- @return node  head   The head of the new node list.
function luatools.node:join(nodes)
    local head = nodes[1]

    -- Add the `next` pointers to each node
    for i = 1, #nodes - 1 do
        nodes[i].next = nodes[i + 1]
    end

    -- Add the `prev` pointers to each node
    node.slide(head)

    return head
end


--= \subsection{\typ{luatools.node:replace}}
--=
--= Replaces a specified node in a node list with another node.

--- @param  head    node The head of the list that contains \typ{find}
--- @param  find    node The node to remove
--- @param  replace node The node to insert
--- @return node    head The new head of the list
function luatools.node:replace(head, find, replace)
    self = self.self

    -- Remove `find` from the list
    local head, current = self.node.remove(head, find)

    -- Insert `replace` in its place
    head, replace = self.node.insert_before(head, current, replace)

    return head
end


--= \subsection{\typ{luatools.node:get_chars}}
--=
--= Converts the contents of a node list to a string.

--- @param  head node The head of the list to convert.
--- @return string -  The contents of the list as a string.
function luatools.node:get_chars(head)
    self = self.self

    local chars = {}

    for n in self.node.traverse(head) do
        if self.node:is(n, "glyph") then
            insert(chars, utf_char(n.char))
        elseif self.node:is(n, "glue") then
            insert(chars, " ")
        elseif n.list or n.replace then
            insert(chars, self.node:get_chars(n.list or n.replace))
        end
    end

    return concat(chars)
end


--= \subsection{\typ{luatools.node:get_next}}
--=
--= Gets the next node in a list that matches the specified criteria.

--- @class _node_get_next -                The criteria to search for.
--- @field is string                       The type of the node to search for.
--- @field direction? "forward"|"backward" The direction to search in.

--- @param  head  node              The head of the list to search.
--- @param  criteria _node_get_next The criteria to search for.
--- @return node? -                 The first node that matches the criteria.
function luatools.node:get_next(head, criteria)
    self = self.self

    local is = criteria.is
    local direction = criteria.direction or "forward"

    if direction == "forward" then
        -- Use the standard traverser, but stop at the first match
        for n in self.node.traverse(head) do
            if self.node:is(n, is) then
                return n
            end
        end
    elseif direction == "backward" and self.fmt.lmtx then
        -- LuaMetaTeX has a builtin reverse traversal function
        for n in self.node.traverse(head, true) do
            if self.node:is(n, is) then
                return n
            end
        end
    elseif direction == "backward" and not self.fmt.lmtx then
        -- For LuaTeX, we need to manually traverse the list in reverse
        local n = head
        while n do
            if self.node:is(n, is) then
                return n
            end
            n = n.prev
        end
    else
        lt.msg:error("Invalid direction: " .. direction)
    end
end


--= \subsection{\typ{luatools.node:map}}
--=
--= Maps a function over a list of nodes.

--- @param  head node                 The head of the list to map over.
--- @param  is   string|boolean       The type of nodes to map over (`true` for
---                                   all).
--- @param  func fun(n:node):...:node The function to apply to each node.
--- @return node head                 The new head of the list.
function luatools.node:map(head, is, func)
    self = self.self

    -- Save all the nodes in a table so that the mapping function can safely
    -- modify the `next` pointers without breaking the traversal.
    local given_nodes = {}
    for n in self.node.traverse(head) do
        insert(given_nodes, n)
    end

    -- Apply the function to each node
    local out_nodes = {}
    for _, n in ipairs(given_nodes) do
        if is == true or self.node:is(n, is) then
            append(out_nodes, { func(n) })
        else
            insert(out_nodes, n)
        end
    end

    return self.node:join(out_nodes)
end


--= \subsection{\typ{luatools.node:colour_list}}
--=
--= Colours a list of nodes.

--- @param  name string The name of the colour
--- @return string? -   The colour's value, in a format-dependent format
local function colour_name_to_value(name)
    if lt.fmt.context then
        return name
    elseif lt.fmt.optex then
        local colour = token.get_macro(name)
        if not colour then
            return
        end

        local rgb = colour:match[[\_setrgbcolor {([%d.]+ [%d.]+ [%d.]+)}]]
        local cmyk = colour:match(
            [[\_setcmykcolor {([%d.]+ [%d.]+ [%d.]+ [%d.]+)}]]
        )

        if rgb then
            return ("%s rg %s RG"):format(rgb, rgb)
        elseif cmyk then
            return ("%s k %s K"):format(cmyk, cmyk)
        end
    elseif lt.fmt.plain or lt.fmt.latex then
        -- `color`, `xcolor`
        local colour = token.get_macro([[\color@]] .. name)

        if colour then
            local xcolor = colour:match[[\xcolor@ {}{([^}]+)}]]

            return xcolor or colour
        end

        -- `l3color`
        -- TODO Not implemented yet
    end
end


--=---------------------------
--= \section{Finalization} ---
--=---------------------------

--= Return the module table so that \lua{local luatools = require "luatools"}
--= works as expected.
return luatools
