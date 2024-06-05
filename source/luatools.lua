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


--= We require \typ{luaotfload} (for \typ{lualibs} and \typ{luatexbase}), but
--= Plain~\LuaTeX{} and Op\TeX{} don't always have it loaded, so we'll load it
--= here if it's not already loaded.
if not package.loaded.luaotfload then
    if optex then
        tex.runtoks(function()
            tex.sprint[[\_initunifonts]]
        end)
    else
        tex.runtoks(function()
            tex.sprint[[\input luaotfload.sty]]
        end)
    end
end


--= \subsection{Local Functions}
--=
--= We're going to use some of the \ConTeXt{} \typ{lualibs} functions quite a
--= bit, so we'll save some (private) local versions of them here.

--= Sets the \typ{__index} metamethod of a table.
local setmetatableindex = table.setmetatableindex

--= Does \lua{{ "a", "b", "c" }} \to \lua{{ a = 1, b = 2, c = 3 }}
local table_swapped = table.swapped

--= Inserts a value into a table.
local insert = table.insert

--= Appends a table to another table, in place.
local append = table.append

--= Merges two tables, without modifying the originals.
local merge = table.imerged

--= Concatenates a table into a string.
local concat = table.concat

--= Unpacks a table
local unpack = table.unpack

--= Slices a table.
local slice = table.sub

--= Gets the character at a given codepoint.
local utf_char = utf8.char

--= Gets the codepoint of a character.
local utf_code = utf8.codepoint

--= Gets the maximum of two values.
local max = math.max


--= \subsection{\typ{luatools}}
--=
--= Now, we define a global table \typ{luatools} to hold all of our defined
--= functions.

--- @class _lt_base      The base for all luatools submodules.
--- @field self luatools The module root.

--- @class luatools: _lt_base       The module class.
--- @field config   luatools.config The module-local data.
--- @field _name_cache table<string, csname_tok> A cache of mangled names.
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

--- @class (exact) luatools.config: _lt_base A table containing module-local
---                                          data.
--- @field name         string  The name of the module.
--- @field ns?          string  The namespace of the module.
--- @field version?     string  The version of the module.
--- @field date?        string  The date of the module, in the format
---                             \typ{YYYY-MM-DD}.
--- @field description? string  A short description of the module.
--- @field debug?       boolean Whether to show debug messages.
--- @field expl?        boolean Whether to use \LaTeX{} expl3 conventions.

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
    local self = table.copy(luatools)
    self.config = config

    -- Make sure that there's always a pointer to the root table
    self.self = self

    for _, submodule in pairs(self) do
        if type(submodule) == "table" then
            rawset(submodule, "self", self)
        end
    end

    -- Print the module name and info to the log file
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
    else
        self.msg:info(
            "Loading version " ..
            (self.config.version or "unknown") ..
            " (" ..
            (self.config.date or "unknown") ..
            ")."
        )
    end

    -- Register the module with the module system
    modules = modules or {}
    modules[self.config.name] = {
        version = self.config.version,
        date    = self.config.date,
        comment = self.config.description,
    }

    -- Initialize the instance-local data
    self._name_cache = {}

    return self
end


--- Ternary operator for LuaTeX and LuaMetaTeX.
--- @generic        T - -
--- @param   luatex T Value if running in \LuaTeX{}.
--- @param   lmtx   T Value if running in LuaMeta\TeX{}.
--- @return  T    out -
local function luatex_lmtx(luatex, lmtx)
    if luatools.fmt.lmtx then
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

--- @class luatools.msg: _lt_base A table containing message functions.
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

        function self.msg:info(msg)
            logs.pushtarget("logfile")
            info(msg)
            logs.poptarget()
        end
    elseif self.fmt.luatexbase then
        -- For Plain and LaTeX, we can use the built-in info reporter.
        local name = self.config.name

        function self.msg:info(msg)
            luatexbase.module_info(name, msg)
        end
    else
        -- OpTeX doesn't have a special info reporter, so we need to do it
        -- manually.
        local start = self.config.name .. " Info: "
        function self.msg:info(msg)
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

        function self.msg:error(msg)
            luatexbase.module_error(name, msg)
        end
    else
        -- For ConTeXt and OpTeX, just use a raw Lua error.
        local start = self.config.name .. " Error: "
        function self.msg:error(msg)
            error(start .. msg, 2)
        end
    end

    self.msg:error(msg)
end


--= Initialize ourself.
--=
--= We need to use \typ{setmetatableindex} here because \typ{luatools.init}
--= creates a copy of the \typ{luatools} table, but we're still adding new
--= functions to it after this. And we need to wait until after we've defined
--= the \typ{msg} submodule so that we can use it to print any messages that
--= we encounter during initialization.
--- @type luatools - -
lt = setmetatableindex(
    luatools.init {
        name        = "luatools",
        ns          = "lt",
        version     = "0.0.0", --%%version
        date        = "2021-07-01", --%%dashdate
        description = "Cross-format Lua helpers."
    },
    function(lt, k)
        local submodule = luatools[k]
        local submodule_indexer = (getmetatable(submodule) or {}).__index

        local v = setmetatableindex(
            submodule,
            function(tt, kk)
                if kk == "self" then
                    return lt
                elseif submodule_indexer then
                    return submodule_indexer(tt, kk)
                end
            end
        )
        lt[k] = v
        return v
    end
)


--=------------------------
--= \section{Utilities} ---
--=------------------------
--=
--= Here, we define some general-purpose utility functions.

--- @class luatools.util: _lt_base A table containing utility functions.
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
--- @alias user_tok luatex.token

--= Next, given a csname as a string, we can easily get the underlying token
--= object.
--- @alias csname_tok string -

--= Finally, a token can be represented as the the table \lua{{cmd, chr, cs}},
--= where \typ{cmd} is the command code, \typ{chr} is the character code, and
--= \typ{cs} is an index into the \TeX{} hash table.
--- @class (exact) tab_tok: table A table representing a token.
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
--- @alias tab_toklist tab_tok[] -

--= The other way to represent a token list is as a string that is tokenized by
--= TeX in a function-dependent manner. These strings are generally processed
--= either using the current catcode regime or using the “\tex{meaning}”
--= catcodes.
--- @alias str_toklist string -

--- @alias any_toklist tab_toklist|str_toklist|user_tok[] - Any type of token list.


--= \subsection{\typ{luatools.token}}
--=
--= We define a table \typ{luatools.token} to hold all of our token functions.
--- @class luatools.token: _lt_base    A table containing token functions.
--- @field [string] function<any, any> A wrapper around the \LuaTeX{}
---                                    \typ{token} functions.
luatools.token = setmetatableindex(function(t, k)
--= LuaMeta\TeX{} removes underscores from most of its \typ{token} functions, so
--= we'll try removing any underscores if we can't find the function.
    local v = token[k] or token[k:gsub("_", "")]
    t[k] = v
    return v
end)

--= Add some useful aliases
luatools.token.scan_integer = luatools.token.scan_int


--= \subsection{\typ{luatools.token:_run}}
--= Runs the given function or token list register inside a new “local \TeX{}
--= run”.
--- @type fun(func: function<nil, nil>): nil
luatools.token._run = luatex_lmtx(tex.runtoks, tex.runlocal)


--= \subsection{\typ{luatools.token:new}}
--=
--= Creates a new token.

--= Some special constants used for macro tokens
local STRING_OFFSET_BITS = 21
local CS_TOKEN_FLAG = 0x1FFFFFFF

--= Creates a token from a character code and a command code.
local token_create_chrcmd = token.new

--= Creates a token from a \typ{csname_tok}.
local token_create_csname = token.create

--= Creates a \typ{user_tok} from an \typ{any_tok}.
---
--- @param  tok any_tok The token to create.
--- @return user_tok -  The created token.
local function token_create_any(tok)
    local tok_type = type(tok)

    -- \typ{user_tok}
    if tok_type == "userdata" and lt.util:type(tok) == "luatex.token" then
        return tok

    -- \typ{csname_tok}
    elseif tok_type == "string" then
        return token_create_csname(tok)

    -- \typ{tab_tok} for a non-macro call token
    elseif tok_type == "table" and (not tok[3] or tok[3] == 0) then
        return token_create_chrcmd(tok[2], tok[1])

    -- \typ{tab_tok} for a macro call token
    elseif tok_type == "table" and tok[3] ~= 0 then
        -- There's no official interface to create a macro token
        -- from Lua, so we need to do it manually.
        return token_create_chrcmd(
            tok[3] + CS_TOKEN_FLAG - (tok[1] << STRING_OFFSET_BITS),
            tok[1]
        )
    else
        lt.msg:error("Invalid token type: " .. lt.util:type(tok))
        return --- @diagnostic disable-line: missing-return-value
    end
end


--- @param  chr integer The character code of the token.
--- @param  cmd integer The command code of the token.
--- @return user_tok -  The created token.
--- @overload fun(self: self, chr: any_tok): user_tok
function luatools.token:new(chr, cmd)
    if cmd then
        return token_create_chrcmd(chr, cmd)
    else
        return token_create_any(chr --[[@as any_tok]])
    end
end


--= \subsection{\typ{luatools.token:push}}
--=
--= Pushes a token list onto the input stack.

--- @param  in_toklist any_toklist The token list to push.
--- @return nil  - -
function luatools.token:push(in_toklist)
    self = self.self

    local out_toklist = {}
    if type(in_toklist) == "string" then
        -- Handle \typ{str_toklist}s by splitting them into lines and then
        -- passing it to \lua{tex.tprint}, which tokenizes the lines using the
        -- current catcodes.
        out_toklist = in_toklist:splitlines()

        tex.tprint(out_toklist)
    else
        -- Convert \typ{tab_toklist}s into \typ{user_tok[]}s.
        for _, tok in ipairs(in_toklist) do
            insert(out_toklist, self.token:new(tok))
        end

        self.token.put_next(out_toklist)
    end
end


--= \subsection{\typ{luatools.token:run}}
--=
--= Runs a piece of \TeX{} code.

--- @param  code any_toklist The code to run.
--- @return nil  - -
function luatools.token:run(code)
    self = self.self

    self.token._run(function()
        self.token:push(code)
    end)
end


--= \subsection{\typ{luatools.token.cmd}}
--=
--= Gets the internal “command code”, indexed by the command name.
luatools.token.cmd = table_swapped(token.commands())

-- We need to add a few extra command codes that aren't in the builtin table
lt.token.cmd["escape"]        = lt.token.cmd["relax"]
lt.token.cmd["min_char_code"] = lt.token.cmd["left_brace"]
lt.token.cmd["out_param"]     = lt.token.cmd["car_ret"]
lt.token.cmd["ignore"]        = lt.token.cmd["endv"]
lt.token.cmd["active_char"]   = lt.token.cmd["par_end"]
lt.token.cmd["match"]         = lt.token.cmd["par_end"]
lt.token.cmd["comment"]       = lt.token.cmd["stop"]
lt.token.cmd["end_match"]     = lt.token.cmd["stop"]
lt.token.cmd["invalid_char"]  = lt.token.cmd["delim_num"]
lt.token.cmd["max_char_code"] = lt.token.cmd["delim_num"]


--= \subsection{\typ{luatools.token.cached}}
--=
--= Gets a \TeX{} token by name and caches it for later.

--- @type fun(name: csname_tok): boolean
local token_defined = luatex_lmtx(token.is_defined, token.isdefined)

--- @type table<string, user_tok?> - -
luatools.token.cached = lt.util:memoized(function(csname)
    -- We don't want to cache undefined tokens
    if token_defined(csname) then
        return lt.token:new(csname)
    end
end)


--= \subsection{\typ{luatools.token:set_csname}}
--=
--= Sets the given csname to be equal to the given token.
--=
--= There's surprisingly no way to define a csname with arbitrary tokens from
--= Lua, so we instead have to use the \tex{let} primitive from \TeX{}.

--- @param  name  name_params  The name of the csname.
--- @param  tok   any_tok      The token to set the csname to.
--- @param  scope scope        -
--- @return user_tok -         The created token.
function luatools.token:set_csname(name, tok, scope)
    self = self.self

    local csname = self.tex:mangle_name(name)

    -- We need to define a token with the provided csname first, otherwise we
    -- get an `undefined_cs`-type token, which can't be passed to TeX without
    -- throwing an error.
    if not token_defined(csname) then
        self.token.set_char(csname, 0)
    end

    -- We can't mix strings and tokens together in a token list, so if we're
    -- given a `csname_tok`, we need to convert it to a `user_tok` first.
    if type(tok) == "string" then
        tok = self.token:new(tok)
    end

    local toks = {
        self.token.cached["let"],
        self.token:new(csname),
        tok
    }

    if scope == "global" then
        insert(toks, 1, self.token.cached["global"])
    end

    self.token:run(toks)

    return self.token:new(csname)
end


--= \subsection{Primitive Tokens}
--=
--= In order to make sure that \lua{lt.token.cached["PRIMITIVE"]} always works,
--= we need to pre-cache all of the primitives with their canonical names.
do
    local primitives = tex.primitives()

    -- \lua{tex.enableprimitives} always defines the primitives globally, so
    -- to make sure that we don't interfere with anything else, we'll use a
    -- private prefix.
    local prefix = "__luatools_primitive_"
    tex.enableprimitives(prefix, primitives)

    -- Now we can cache all of the primitives
    for _, csname in ipairs(primitives) do
        -- We copy the underlying values representing the primitive instead of
        -- copying the primitive itself, just in case someone decides to
        -- somehow redefine one of our private csnames.
        local primitive = lt.token:new(prefix .. csname)
        luatools.token.cached[csname] = lt.token:new(
            primitive.mode, primitive.command
        )
    end

    -- And some special non-primitive tokens
    luatools.token.cached["{"] = lt.token:new(
        utf_code "{", lt.token.cmd["left_brace"]
    )
    luatools.token.cached["}"] = lt.token:new(
        utf_code "}", lt.token.cmd["right_brace"]
    )
    luatools.token.cached["undefined"] = lt.token:new(
        utf_code "!", lt.token.cmd["undefined_cs"]
    )
    luatools.token.cached["has_csname"] = lt.token:new(prefix .. "relax")
end


--= \subsection{\typ{luatools.token:set_toks_register}}
--=
--= Sets a \tex{toks} register to the given token list.

--= A \typ{user_defined} whatsit node that we can use to convert between
--= various internal \LuaTeX{} datatypes.
local scratch_user_whatsit = node.new("whatsit", "user_defined") --[[
    @as user_whatsit]]

--= The possible types of a \tex{user_defined} whatsit node.
--- @enum user_whatsit_type
local user_whatsit_types = {
    attribute = utf_code "a",
    integer   = utf_code "d",
    lua       = utf_code "l",
    node      = utf_code "n",
    string    = utf_code "s",
    toklist   = utf_code "t",
}

--= Stores a \typ{tab_toklist} in a “fake” \tex{toks} register (without giving
--= it a name).
---
--- @param  toklist tab_toklist The token list to store.
--- @return user_tok -          A token representing the fake register.
local function toklist_to_faketoks(toklist)
    local temp_char = lt.tex:mangle_name {
        name = "toklist_to_faketoks_tmp",
        visibility = "private",
        type = "weird",
    }

    -- We need to save the token list into the \TeX{} hash table to be able to
    -- access it in \TeX{}.
    scratch_user_whatsit.type = user_whatsit_types.toklist
    scratch_user_whatsit.value = toklist
    scratch_user_whatsit.type = user_whatsit_types.integer
    local tok_id = scratch_user_whatsit.value


    -- \TeX{} expects two levels of indirection for a \tex{toks} token, so we
    -- first point a \tex{chardef} token to the token stored above.
    lt.token.set_char(temp_char, tok_id)

    -- Then, we create a “fake” \tex{toks} token that points to the
    -- \tex{chardef} token.
    return lt.token:new(
        lt.token:new(temp_char).tok - CS_TOKEN_FLAG,
        lt.token.cmd["assign_toks"]
    )
end


--= Copies a fake \tex{toks} token into a real \tex{toks} register.
--=
--= The token created by \typ{let_csname_token} is a semi-valid \tex{toks}
--= register: it behaves like a \tex{toks} register with \tex{the} and similar,
--= but it gives a (mostly harmless) error with \tex{show} and \tex{meaning}.
--= To fix this, we copy the token's contents into a real \tex{toks} register.
---
--- @param  name     csname_tok The name of the register.
--- @param  fake_tok user_tok   The fake \tex{toks} register.
--- @return nil  -          -
local function token_to_toks_register(name, fake_tok)
    -- Clear the register
    tex.toks[name] = ""

    local fake_tok_name = lt.tex:mangle_name {
        name = "token_to_toks_register_tmp",
        type = "toks",
        visibility = "private",
    }
    lt.token:set_csname(fake_tok_name, fake_tok)

    -- Prepend the fake \tex{toks} register onto the empty real one, giving
    -- us a real \tex{toks} register with the correct value.
    lt.token:run {
        lt.token.cached["tokspre"],
        lt.token:new(name),
        lt.token:new(fake_tok_name),
    }
end


--- @param  name    name_params The name of the register.
--- @param  toklist tab_toklist The token list to store.
--- @return nil -          -
function luatools.token:set_toks_register(name, toklist)
    self = self.self

    local csname = self.tex:mangle_name(name)
    local fake_tok = toklist_to_faketoks(toklist)
    token_to_toks_register(csname, fake_tok)
end


--= \subsection{\typ{luatools.token:toklist_to_str}}
--=
--= Converts a token list to a string.

--- @param  toklist tab_toklist The token list to convert.
--- @return string -            The string representation of the token list.
function luatools.token:toklist_to_str(toklist)
    self = self.self

    -- Allocate a new token register
    local name = lt.tex:new_register {
        name = "toklist_to_str_tmp",
        type = "toks",
        visibility = "private",
    }

    -- Store the token list in the register
    self.token:set_toks_register(name, toklist)

    return lt.tex[name] --[[@as string]]
end


--= \subsection{\typ{luatools.token:to_tab_toklist}}
--=
--= Converts \typ{any_toklist}s into \typ{tab_toklist}s.

--- @param  in_toklist any_toklist The token list to convert.
--- @return tab_toklist -       The converted token list.
function luatools.token:to_tab_toklist(in_toklist)
    self = self.self

    if type(in_toklist) == "string" then
        -- Get the current catcodes
        local current_cctab = self.tex.catcodetable --[[@as integer]]

        -- Tokenize the string and store it in a \tex{toks} register
        local register_csname = self.tex:new_register {
            name = "to_tab_toklist_tmp",
            type = "toks",
        }
        tex.scantoks(register_csname, current_cctab, in_toklist)

        -- Dereference the pointers
        local outer_ptr = self.token:new(register_csname).mode
        local inner_ptr = (lt.token:new { 0, 0, outer_ptr }).mode

        -- Extract the tokens
        scratch_user_whatsit.type  = user_whatsit_types.integer
        scratch_user_whatsit.value = inner_ptr
        scratch_user_whatsit.type  = user_whatsit_types.toklist
        local out_toklist = scratch_user_whatsit.value --[[@as tab_toklist]]

        return slice(out_toklist, 2)
    else
        local out_toklist = {}
        for _, any_tok in ipairs(in_toklist) do
            if type(any_tok) == "table" then
                insert(out_toklist, any_tok)
            else
                local user_tok = self.token:new(any_tok)
                insert(out_toklist, {
                    user_tok.command,
                    user_tok.mode,
                    max(user_tok.tok - CS_TOKEN_FLAG, 0)
                })
            end
        end

        return out_toklist
    end
end


--=--------------------------------
--= \section{\TeX{} Interfaces} ---
--=--------------------------------
--=
--= The \typ{luatools.tex} table is the high-level interface for working with
--= \TeX{} tokens and registers.

-- The typechecker doesn't like the `__index` metamethod, so we need to manually
-- forward-declare the types of the function methods.

--- @class luatools.tex: _lt_base A table containing \TeX{} functions.
--- @field self luatools          The module root.
--- @field mangle_name fun(self:self, params: name_params): string
--- @field new_register fun(self:self, params: name_params): string
--- @field _get_token fun(self:self, name: name_params): user_tok, csname_tok
--- @field [string] ((fun(...):nil))|integer|string|user_tok|nil -
---        Accessor for \TeX{} registers and macros.
luatools.tex = {}


--= \subsection{\typ{luatools.tex:mangle_name}}
--=
--= Every \TeX{} format uses different naming conventions for their user-defined
--= macros and registers, which makes it quite tricky to write cross-format
--= code. This function converts a friendly “Lua” name into a format-specific
--= “\TeX{}” name.

--= Right now, we'll only support \TeX{} registers with the following types:
--- @alias register_type
--- | "attribute"
--- | "catcodetable"
--- | "count"
--- | "dimen"
--- | "toks"
--- | "whatsit"

--= For consistency, we're using the standard \TeX{} names for the types, so
--= we'll need to convert them to the expl3 names if we're using expl3.
--- @type table<register_type, string> - -
local expl_types = {
    attribute    = "attr",
    catcodetable = "cctab",
    count        = "int",
    dimen        = "dim",
    toks         = "toks",
    whatsit      = "whatsit",
}

--= \LuaTeX{} gives \tex{REGISTERdef}ed tokens specific command codes which
--= we'll unfortunately need to hardcode.
--- @type table<register_type, string> - -
local texname_to_cmdname = {
    attribute    = "assign_attr",
    catcodetable = "char_given", -- close enough...
    count        = "assign_int",
    dimen        = "assign_dimen",
    toks         = "assign_toks",
    whatsit      = "char_given",
}

local cmdname_to_texname = table_swapped(texname_to_cmdname)

--= Macros aren't registers, but they still have a csname, so we'll support them
--= in \lua{luatools.tex} too. We'll also define the type \lua{"weird"} for
--= weird user-defined types.
--- @alias tex_type register_type|"macro"|"weird" -

--- @class (exact) _name_params The parameters for the mangled name.
--- @field name        string   The name to mangle.
--- @field type?       tex_type The \TeX{} type that the name points at.
--- @field arguments?  string   An expl3 macro “signature” (the characters after
---                             \typ{:} in the macro name). (Default: \lua{""})
--- @field scope?      scope    Global or local? (Default: \lua{"local"})
--- @field visibility? "public"|"private" Public or private? (Default:
---                                       \lua{"private"})

--- @alias name_params _name_params|string Either mangle a name using the
---                                        given parameters, or just return the
---                                        passed string as-is.

--- @param  params name_params The parameters for the mangled name.
--- @return string -           The mangled name.
function luatools.tex:mangle_name(params)
    self = self.self

    -- Pass through the name as-is if we're given a string
    if type(params) == "string" then
        return params
    end

    -- If we've already defined this name, then just return it from the cache.
    if self._name_cache[params.name] then
        return self._name_cache[params.name]
    end

    -- Set the defaults
    local name = params.name
    params.arguments  = params.arguments  or ""
    params.scope      = params.scope      or "local"
    params.visibility = params.visibility or "private"

    if self.config.expl and not params.type then
        lt.msg:error("You must provide a type when using expl3.")
        return --- @diagnostic disable-line: missing-return-value
    end

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

    -- Store the mangled name in the cache, only if the token it references is
    -- defined. (We'll sometimes use this function to see if a mangled name is
    -- already defined, so we can't cache it unconditionally.)
    if token_defined(name) then
        self._name_cache[params.name] = name
    end

    return name
end


--= \subsection{\typ{luatools.tex:new_register}}
--=
--= Creates a new \TeX{} register with the given parameters.

--- @param  params name_params  The parameters for the register.
--- @return csname_tok name     The mangled name of the register.
function luatools.tex:new_register(params)
    self = self.self

    if params.type == "macro" then
        lt.msg:error("Use ``lt.macro:define'' to define new macros.")
        return --- @diagnostic disable-line: missing-return-value
    end

    -- Get the mangled name
    local name = self._name_cache[params.name] or
                 self.tex:mangle_name(params)

    -- Check if the register is already defined
    local tok = self.token.cached[name]
    if tok then
        if tok.cmdname == texname_to_cmdname[params.type] then
            -- The register is already defined with the correct type, so there's
            -- nothing to do here.
            return name
        else
            lt.msg:error("Register ``" .. name .. "'' already defined.")
        end
    else
        -- If it isn't already defined, then we need to define it.
        self.token.set_char(name, 0)
    end

    if params.type == "whatsit" then
        -- Whatsits aren't registers at all, but we still need an allocator for
        -- them.
        local value  --- @type integer
        if self.fmt.luatexbase then
            value = luatexbase.new_whatsit(name)
        elseif self.fmt.context then
            value = nodes.pool.userids[name]
        elseif self.fmt.optex then
            -- OpTeX doesn't have an allocator for whatsits, so we'll use a
            -- SHA-2 hash of the name instead.
            local low, high = sha2.digest256(name):byte(1, 2)
            value = high * 256 + low
        end

        -- Make a \tex{chardef} token so that we can treat it like a register
        self.token.set_char(name, value)
    else
        -- We've got a “real” register, so let's define it.
        self.token:run {
            self.token.cached["new" .. params.type],
            self.token:new(name)
        }
    end

    -- Cache the name
    self._name_cache[params.name] = name

    return name
end


--= \subsection{\typ{luatools.tex:_get_token}}
--=
--= Gets the token corresponding to the given name.
--=
--= To provide a user-friendly interface, we take only the variable name, then
--= check all possible mangled names for the variable.

--- Helper function for \lua{luatools.tex:_get_token}.
--- @param  given   string The requested variable name.
--- @param  mangled string The mangled variable name.
--- @return user_tok? -    The token corresponding to the variable.
function luatools.tex:_get_token_aux(given, mangled)
    self = self.self

    local tok = self.token.cached[mangled]
    if tok then
        self._name_cache[given] = mangled
        return tok
    end
end


--- @param  name name_params    The name of the variable.
--- @return user_tok?   tok     The token corresponding to the variable.
--- @return csname_tok? mangled The mangled name of the variable.
function luatools.tex:_get_token(name)
    self = self.self

    -- Initialization
    local mangled --- @type csname_tok
    local tok --- @type user_tok?

    -- If we're given a table, then mangle the name using those exact
    -- specifications
    if type(name) == "table" then
        local params = name
        name = params.name

        mangled = self.tex:mangle_name(params)
        tok = self.tex:_get_token_aux(name, mangled)
        return tok, mangled

    -- We need this check since otherwise the typechecker gets upset
    elseif type(name) ~= "string" then
        return --- @diagnostic disable-line: missing-return-value
    end

    -- Check the cache
    mangled = self._name_cache[name]
    if mangled then
        return self.token.cached[mangled], mangled
    end

    -- Check for the exact name
    mangled = name
    tok = self.tex:_get_token_aux(name, mangled)
    if tok then return tok, mangled end

    if self.config.expl then
        -- We need to loop over `{public, private} × {local, global} × {*types}`
        for _, visibility in ipairs { "public", "private" } do
            for _, scope in ipairs { "local", "global" } do
                local base, arguments = name:match("^(.-):(.*)$")
                if arguments then
                    -- Macros
                    mangled = self.tex:mangle_name {
                        name = base,
                        type = "macro",
                        arguments = arguments,
                        scope = scope,
                        visibility = visibility
                    }
                    tok = self.tex:_get_token_aux(name, mangled)
                    if tok then return tok, mangled end
                else
                    -- Registers
                    for type, _ in pairs(expl_types) do
                        mangled = self.tex:mangle_name {
                            name = name,
                            type = type,
                            scope = scope,
                            visibility = visibility
                        }
                        tok = self.tex:_get_token_aux(name, mangled)
                        if tok then return tok, mangled end
                    end
                end
            end
        end
    else
        -- Check for a private variable
        mangled = self.tex:mangle_name {
            name = name,
            visibility = "private"
        }
        tok = self.tex:_get_token_aux(name, mangled)
        if tok then return tok, mangled end

        -- Check for a public variable
        mangled = self.tex:mangle_name {
            name = name,
            visibility = "public"
        }
        tok = self.tex:_get_token_aux(name, mangled)
        if tok then return tok, mangled end

    end --- @diagnostic disable-line: missing-return
end


--= \subsection{\typ{luatools.tex:_get}}
--=
--= Gets the value of the given register, or a function that calls the given
--= macro.

--- @param  name name_params The name of the register or macro.
--- @return integer|(fun(...):nil)|user_tok? - The contents of the register or
---                                            macro.
function luatools.tex:_get(name)
    self = self.self
    local outer_self = self

    local tok = self.tex:_get_token(name)
    if not tok then
        return nil
    end

    -- Macro call
    local cmdname = tok.cmdname
    if cmdname:match("call") then
        return function(...)
            local in_args = {...}
            local out_args = {}

            local bgroup, egroup
            if type(in_args[1]) == "string" then
                bgroup = "{"
                egroup = "}"
            else
                bgroup = outer_self.token.cached["{"]
                egroup = outer_self.token.cached["}"]
            end

            for _, arg in ipairs(in_args) do
                insert(out_args, bgroup)
                insert(out_args, arg)
                insert(out_args, egroup)
            end

            outer_self.token:run {
                tok,
                unpack(out_args)
            }
        end
    end

    -- Register
    local register_type = cmdname_to_texname[cmdname]
    if register_type then
        if tok.index then
            -- Regular register
            return tex[register_type][tok.index]
        else
            -- Internal parameter
            return tex[name]
        end
    end

    -- Fallback to running the given token
    return function(...)
        outer_self.token:run {
            tok,
            ...
        }
    end
end


--= \subsection{\typ{luatools.tex:_set}}
--=
--= Sets the value of the given register, or sets the given csname to a token.

local dimen_to_sp = tex.sp

--- @param  name name_params                 The name of the register or csname.
--- @param  val  any_tok|integer|tab_toklist The value to set the register or
---                                          csname to.
--- @return nil  -               -
function luatools.tex:_set(name, val)
    self = self.self

    local tok, name = self.tex:_get_token(name)
    if not tok then
        self.msg:error("Unknown variable ``" .. name .. "''.")
        return
    end

    local register_type = cmdname_to_texname[tok.cmdname]
    local val_type = self.util:type(val)

    -- Set directly
    if ((register_type == "dimen" or register_type == "count")
        and val_type == "number") or
       (register_type == "toks" and val_type == "string")
    then
        if tok.index then
            -- Regular register
            tex[register_type][tok.index] = val
        else
            -- Internal parameter
            tex[name] = val
        end
        return
    end

    -- Handle dimensions given like "2em"
    if register_type == "dimen" and val_type == "string" then
        --- @cast val string
        tex[register_type][tok.index] = dimen_to_sp(val)
        return
    end

    -- Handle setting a \tex{toks} register to a token table
    if register_type == "toks" and val_type == "table" then
        --- @cast val tab_toklist
        self.token:set_toks_register(name, val)
        return
    end

    -- Handle \tex{partokenname}
    if tok.command == self.token.cmd["partoken_name"] and
       val_type == "luatex.token"
    then
        self.token:run {
            tok,
            val --[[@as user_tok]]
        }
        return
    end

    self.msg:error("Invalid value for variable ``" .. name .. "''.")
end


--= Make \typ{luatools.tex:_get} and \typ{luatools.tex:_set} metamethods on
--= \typ{luatools.tex}.
setmetatable(luatools.tex, {
    __index = luatools.tex._get,
    __newindex = luatools.tex._set,
})


--=---------------------
--= \section{Macros} ---
--=---------------------
--=
--= The \typ{luatools.macro} table contains functions for working with \TeX{}
--= macros (tokens defined by \tex{def}).

--- @class luatools.macro: _lt_base A table containing macro functions.
luatools.macro = {}


--= \subsection{\typ{luatools.macro:unexpanded}}
--=
--= Gets the raw, unexpanded “replacement text” of a macro as a string.

--- @param  name name_params The csname of the macro.
--- @return string -         The raw replacement text.
function luatools.macro:unexpanded(name)
    self = self.self

    local csname = self.tex:mangle_name(name)

    return self.token.get_macro(csname)
end


--= \subsection{\typ{luatools.macro:expanded_toks}}
--=
--= Gets the expanded “replacement text” of a macro as a token list, like
--= \tex{expanded} would do.

--- @param  name name_params The csname of the macro.
--- @return tab_toklist -    The expanded replacement text.
function luatools.macro:expanded_toks(name)
    self = self.self

    local csname = self.tex:mangle_name(name)

    -- To correctly expand \LaTeX{} \tex{protect}ed macros, we need to redefine
    -- \tex{protect} before expanding the macro, then restore it afterwards.
    -- There's no way to save a token's definition from Lua, so we'll need to
    -- copy it to a new csname.
    local saved_protect = lt.tex:mangle_name {
        name = "expanded_toks_tmp",
        visibility = "private",
        type = "weird",
    }

    -- Push the new definition of \tex{protect}
    self.token:set_csname(saved_protect, self.token:new("protect"))
    self.token:set_csname("protect", self.token.cached["string"])

    -- (1) To expand the macro, we first push the macro's csname and a closing
    -- brace onto the token stack:
    --     grabbed: nil
    --     stack  : <\MACRO> <}> <rest of document>
    --
    -- (2) Next, we expand and grab the first token from the stack:
    --     grabbed: <1st \MACRO body>
    --     stack  : <2nd \MACRO body> <3rd body> <... body> <}> <rest>
    --
    -- (3) Then, we push an opening brace and the grabbed first token back onto
    -- the stack:
    --     grabbed: nil
    --     stack  : <{> <1st body> <2nd body> <3rd body> <... body> <}> <rest>
    --
    -- (4) Finally, we expand and grab a complete token list:
    --     grabbed: <1st body> <2nd body> <3rd body> <... body>
    --     stack  : <rest of document>
    --
    -- We need (2) and (3) in case we are given a \tex{protected} macro, which
    -- wouldn't expand at all in (4) otherwise.
    local out_toklist --- @type tab_toklist
    self.token._run(function()
        -- (1)
        self.token:push {
            self.token:new(csname),
            self.token.cached["}"]
        }
        -- (2)
        local first = self.token.scan_token()
        -- (3)
        self.token:push {
            self.token.cached["{"],
            first,
        }
        -- (4)
        out_toklist = self.token.scan_toks(false, true)
    end)

    -- Pop the definition of \tex{protect}
    self.token:set_csname("protect", self.token:new(saved_protect))

    return out_toklist
end


--= \subsection{\typ{luatools.macro:expanded}}
--=
--= Gets the expanded “replacement text” of a macro as a string, like
--= \tex{message} would do.

--- @param  name name_params The csname of the macro.
--- @return string -         The expanded replacement text.
function luatools.macro:expanded(name)
    self = self.self

    local csname = self.tex:mangle_name(name)
    local toks = self.macro:expanded_toks(csname)
    local str = self.token:toklist_to_str(toks)

    return str
end


--= \subsection{\typ{luatools.macro:super_expanded}}
--=
--= Fully-expands a macro by typesetting it in a box and reading the box's
--= contents.

--- @param  name  name_params The csname of the macro.
--- @param  safe? boolean     Whether to use \tex{hbox} (unsafe) or \tex{hpack}
---                           (safe). (Default: \lua{false})
--- @return string -          The fully-expanded replacement text.
function luatools.macro:super_expanded(name, safe)
    self = self.self

    local csname = self.tex:mangle_name(name)

    local hbox
    if safe then
        hbox = self.token.cached["hpack"]
    else
        hbox = self.token.cached["hbox"]
    end

    local out_node --- @type node
    self.token._run(function()
        self.token:push {
            hbox,
            self.token.cached["{"],
            self.token:new(csname),
            self.token.cached["}"]
        }
        out_node = self.token.scan_list()
    end)

    return self.node:to_str(out_node)
end


--= \subsection{\typ{luatools.macro:to_toklist}}
--=
--= Gets the raw, unexpanded “replacement text” of a macro as an array of
--= tokens.

--= Gets a token list representing the full \tex{meaning} of a macro, with
--= with catcodes and such intact.
---
--- @param  name csname_tok The csname of the macro.
--- @return tab_toklist -   The \tex{meaning} of the macro.
local function macro_to_toklist(name)
    -- Get the token for the macro
    local macro = lt.token:new(name)

    if not macro.cmdname:match("call") then
        lt.msg:error("``" .. name .. "'' is not a macro.")
        return --- @diagnostic disable-line: missing-return-value
    end

    -- We use a \typ{user_defined} whatsit node to convert the macro to a token
    -- list.
    scratch_user_whatsit.type  = user_whatsit_types.integer
    scratch_user_whatsit.value = macro.mode
    scratch_user_whatsit.type  = user_whatsit_types.toklist

    return scratch_user_whatsit.value --[[@as tab_toklist]]
end


-- Character code constants
local first_digit_chrcode     = utf_code "0"
local hash_chrcode            = utf_code "#"

--= Splits a macro definition token list into its parameters and its
--= replacement text.
--- @param  toklist tab_toklist The token list of the macro's \tex{meaning}.
--- @param  raw?    "raw"       Whether to convert the internal tokens into a
---                             more user-friendly format.
--- @return tab_toklist params  The parameters of the macro.
--- @return tab_toklist body    The replacement text of the macro.
local function split_macro_meaning(toklist, raw)
    local stop_index
    local args_count = 0

    for i, t in ipairs(toklist) do
        -- Separator between parameters and replacement text (represented by
        -- "->" inside of \meaning).
        if t[1] == lt.token.cmd["end_match"] then
            stop_index = i
        end

        -- Don't convert any tokens if we're in raw mode
        if raw then
            if stop_index then
                break
            end

        -- Convert a macro parameter token in the body back into a "#"
        -- token.
        elseif t[1] == lt.token.cmd["mac_param"] and t[3] == 0 then
            insert(
                toklist,
                i + 1,
                { lt.token.cmd["mac_param"], hash_chrcode, 1 }
            )
        elseif t[1] == lt.token.cmd["mac_param"] and t[3] == 1 then
            t[3] = 0

        -- Convert a macro parameter token in the body back into a <digit>
        -- token.
        elseif t[1] == lt.token.cmd["out_param"] then
            insert(
                toklist,
                i + 1,
                { lt.token.cmd["other_char"], first_digit_chrcode + t[2], 0 }
            )
            t[2] = hash_chrcode
            t[1] = lt.token.cmd["mac_param"]

        -- Convert a macro parameter token in the parameters back into a
        -- pair of tokens {"#", <digit>}.
        elseif t[1] == lt.token.cmd["match"] then
            args_count = args_count + 1
            t[1] = lt.token.cmd["mac_param"]
            t[2] = hash_chrcode
            insert(
                toklist,
                i + 1,
                { lt.token.cmd["other_char"], first_digit_chrcode + args_count, 0 }
            )
        end
    end

    -- Split the token table
    return slice(toklist, 2,              stop_index - 1),
           slice(toklist, stop_index + 1, nil           )
end


--- @param  name name_params   The csname of the macro.
--- @param  raw? "raw"         Whether to convert the internal tokens into a
---                            more user-friendly format.
--- @return tab_toklist params The parameters of the macro.
--- @return tab_toklist body   The replacement text of the macro.
function luatools.macro:to_toklist(name, raw)
    self = self.self

    local csname = self.tex:mangle_name(name)
    local meaning = macro_to_toklist(csname)
    local params, body = split_macro_meaning(meaning, raw)

    return params, body
end


--= \subsection{\typ{luatools.macro:from_toklist}}
--=
--= Converts a token list into a macro definition. Note that you need to pass
--= the token list in in \emph{exactly} the form that \TeX{} internally expects
--= it to be in---this mainly applies to the arguments, which should be entered
--= as \lua{{cmd.par_end, chr "#"}} in the parameters and \lua{{cmd.car_ret, n}}
--= in the replacement text.

--- @param  name   name_params  The csname of the macro.
--- @param  params tab_toklist  The parameters of the macro.
--- @param  body   tab_toklist  The replacement text of the macro.
--- @param  type?  string       The type of the macro. (Default: \lua{"call"})
--- @return user_tok macro      The token representing the new macro.
function luatools.macro:from_toklist(name, params, body, type)
    self = self.self

    -- Merge the parameters and the body
    local toklist = {}
    append(toklist, params)
    insert(toklist, token_create_chrcmd(0, self.token.cmd["end_match"]))
    append(toklist, body)

    -- Save the token list into \typ{eqtb} and get a pointer to it
    scratch_user_whatsit.type  = user_whatsit_types.toklist
    scratch_user_whatsit.value = toklist
    scratch_user_whatsit.type  = user_whatsit_types.integer
    local macro_ptr = scratch_user_whatsit.value --[[@as integer]]

    -- Get the command code for the macro
    local cmd --- @type integer
    if type and self.token.cmd[type] then
        cmd = self.token.cmd[type]
    else
        cmd = self.token.cmd["call"]
    end

    -- Assign the macro to the csname
    local macro_tok = token_create_chrcmd(macro_ptr, cmd)
    return self.token:set_csname(name, macro_tok)
end


--= \subsection{\typ{luatools.macro:define}}
--=
--= Defines a new macro.

--- @alias _arguments
--- | "argument"
--- | "csname"
--- | "dimen"
--- | "float"
--- | "glue"
--- | "integer"
--- | "list"
--- | "string"
--- | "toks"
--- | "word"

--- @class (exact) _macro_define: _name_params The parameters for defining a
---                                            new macro.
--- @field type   nil     (Omit from parent type)
--- @field func   fun(...):nil The Lua function to run when the macro is called.
--- @field exact? boolean Whether to use the exact name provided, or to use the
---                       standard name mangling. (Default: \lua{false})
--- @field arguments? _arguments[] A list of the arguments that the macro takes.
---                                (Default: \lua{{}})
--- @field ["protected"]? boolean  Whether the macro is \tex{protected}.
---                                (Default: \lua{false})

--- @param  params _macro_define The parameters for the new macro.
--- @return user_tok macro       The token representing the new macro.
function luatools.macro:define(params)
    self = self.self

    -- Setup the default parameters
    local arguments = params.arguments or {}
    local func      = params.func

    -- Get the mangled name
    local name
    if params.exact then
        name = params.name
    else
        local mangle_params = table.copy(params)
        mangle_params.type = "macro"
        mangle_params.arguments = string.rep("n", #arguments)
        name = self.tex:mangle_name(mangle_params)
    end

    -- Generate the scanning function
    local scanning_func
    if self.fmt.context then
        -- \ConTeXt{} handles the scanning for us, so there's nothing to do
        -- here.
    elseif #arguments == 0 then
        -- No arguments, so we can just run the function directly
        scanning_func = func
    else
        -- Before we can run the given function, we need to scan for the
        -- requested arguments in \TeX{}, then pass them to the function.

        local scanners = {} --- @type (fun(nil):any)[]
        for _, argument in ipairs(arguments) do
            insert(scanners, self.token["scan_" .. argument])
        end

        -- An intermediate function that properly “scans” for its arguments
        -- in the \TeX{} side.
        scanning_func = function()
            local values = {}
            for _, scanner in ipairs(scanners) do
                insert(values, scanner())
            end

            func(table.unpack(values))
        end
    end

    -- Handle scope and protection
    local extra_params = {}
    if params.scope == "global" then
        insert(extra_params, "global")
    end
    if params.protected then
        insert(extra_params, "protected")
    end

    -- Define the macro
    if self.fmt.optex then
        optex.define_lua_command(name, scanning_func, unpack(extra_params))
    elseif self.fmt.luatexbase then
        local index = luatexbase.new_luafunction(name)
        lua.get_functions_table()[index] = scanning_func
        self.token.set_lua(name, index, unpack(extra_params))
    elseif self.fmt.context then
        interfaces.implement {
            name = name,
            public = true,
            arguments = arguments,
            actions = func,
            protected = params.protected,
        }
    end

    return self.token:new(name)
end


--=-----------------------
--= \section{Verbatim} ---
--=-----------------------
--=
--= The \typ{luatools.verbatim} table contains functions for grabbing
--= untokenized text from \TeX{}.

--- @class luatools.verbatim: _lt_base A table containing verbatim functions.
luatools.verbatim = {}


--= \subsection{Catcode Tables}
--=
--= In order to grab verbatim text, it's best if we can set all possible
--= catcodes to “other” so that the text is not tokenized. To do this, we'll
--= need to create a new catcode table \typ{verbatim_cctab} where all characters
--= are set to “other”, and another catcode table \typ{almost_verb_cctab} where
--= everything except for braces are set to “other”.
--- @type integer, integer
local verbatim_cctab, almost_verb_cctab
do
    -- Create a new catcode table to use for verbatim text
    local verb_csname = lt.tex:new_register {
        name  = "verbatim_catcodes",
        type  = "catcodetable",
        scope = "global",
    }
    local verb_tok = lt.token:new(verb_csname)
    local verb_index = verb_tok.index --- @cast verb_index -nil

    -- And for almost-verbatim text
    local almost_csname = lt.tex:new_register {
        name  = "almost_verbatim_catcodes",
        type  = "catcodetable",
        scope = "global",
    }
    local almost_tok = lt.token:new(almost_csname)
    local almost_index = almost_tok.index --- @cast almost_index -nil

    -- Initialize the catcode tables
    lt.tex.initcatcodetable(verb_tok)
    lt.tex.initcatcodetable(almost_tok)

    for chr = 0, 127 do
        tex.setcatcode(verb_index,   chr, lt.token.cmd["other_char"])
        tex.setcatcode(almost_index, chr, lt.token.cmd["other_char"])
    end

    -- Set the catcodes for the braces
    tex.setcatcode(almost_index, utf_code "{", lt.token.cmd["left_brace"] )
    tex.setcatcode(almost_index, utf_code "}", lt.token.cmd["right_brace"])

    -- Save the index of the catcode table
    verbatim_cctab = verb_index
    almost_verb_cctab = almost_index
end

--= Handle saving and restoring the catcodes.
local push_catcodes, pop_catcodes
local saved_partokenname --- @type user_tok
do
    local par_token_grabber = lt.macro:define {
        name = "partokenname_grabber",
        func = function()
            saved_partokenname = token.get_next()
        end,
    }

    -- Hold the previous values to restore on pop
    local saved_cctab       --- @type integer

    --- Push the catcode table
    --- @param  cctab_index integer The index of the catcode table to use.
    --- @return nil         -       -
    function push_catcodes(cctab_index)
        -- Save the current catcode table
        saved_cctab = lt.tex.catcodetable --[[@as integer]]

        -- Save the current \tex{partokenname}
        tex.print {
            par_token_grabber,
            utf_char(lt.tex.endlinechar --[[@as integer]]),
        }
        lt.token._run(function() end)

        -- Set \tex{partokenname} to something with a csname
        lt.tex.partokenname = lt.token.cached["has_csname"]

        -- Set the new catcode table
        lt.tex.catcodetable = cctab_index
    end

    --- Pop the catcode table
    --- @return nil - -
    function pop_catcodes()
        -- Reset the catcodes and \tex{partokenname}
        lt.tex.catcodetable = saved_cctab
        lt.tex.partokenname = saved_partokenname

        -- Push a dummy token so that \TeX{} is in the appropriate scanning
        -- state.
        lt.token:push { lt.token.cached["relax"] }
    end
end


--= \subsection{\typ{luatools.verbatim:grab_until}}
--=
--= Grabs contents as a verbatim string until the specified tokens are found.

--= Post-processes the grabbed text.
--- @param  text        string The grabbed text.
--- @param  outer_level boolean Whether the text was grabbed at the outer level.
--- @return string      -       The post-processed text.
local function verb_postprocess(text, outer_level)
    -- Replace the \tex{endlinechar} with a newline
    local endlinechar = utf_char(lt.tex.endlinechar --[[@as integer]])
    text = text:gsub(endlinechar, "\n")

    -- Remove extra spaces after a macro if the text was already tokenized
    if not outer_level then
        -- Sort all possible characters into letters and others. We need to do
        -- this dynamically each time since the catcodes might have changed.
        local letters = {}
        local others  = {}

        for charcode = 0, 255 do
            local catcode = tex.catcode[charcode]
            local char    = utf_char(charcode)

            if catcode == lt.token.cmd["letter"] then
                insert(letters, char)
            elseif catcode ~= lt.token.cmd["invalid_char"] then
                insert(others, char)
            end
        end

        -- Switch to the LPeg environment to reduce typing.
        local _ENV = lpeg

        -- Define patterns to match single characters by their catcodes.
        local any_char    = P(1)
        local escape_char = P(utf_char(lt.tex.escapechar --[[@as integer]]))
        local letters     = S(concat(letters))
        local others      = S(concat(others) )

        -- A “control symbol” is the escape character followed by a single
        -- non-letter character.
        local control_symbol = escape_char * others

        -- A “control word” is the escape character followed by a sequence of
        -- letters.
        local control_word = escape_char * letters^1

        -- A “control sequence” is either a “control word” or a “control
        -- symbol”.
        local control_sequence = control_word + control_symbol

        -- \TeX{} adds a single space after control words; we want to remove
        -- this.
        local remove_space       = (P " ") / ""
        local control_word_space = control_word * remove_space

        -- Find \tex{the}\tex{partokenname}
        local partoken_csname = (saved_partokenname or {}).csname or P(false)
        local partoken        = escape_char * partoken_csname * P(" ")

        -- One newline in the source maps to a plain space (unfortunately), two
        -- newlines map to a single partoken, three newlines map to two
        -- tokens, and so on.
        local partoken_nl = Cf(
            Cc(false) * C(partoken)^1,
            function(so_far, new)
                if so_far then
                    return so_far .. "\n"
                else
                    return "\n\n"
                end
            end
        )

        -- We want to match this pattern anywhere in the text.
        local anywhere = (partoken_nl + control_word_space + any_char)^0

        -- Do the actual replacement
        text = match(Cs(anywhere), text)
    end

    return text
end


do
    -- Here, we define a Lua macro that we can place in the body of the
    -- delimited macro so that we can send the grabbed text back to Lua.
    local verb_grabber_out --- @type string?

    local inner_macro = lt.macro:define {
        name = "inner_verb_grabber",
        func = function()
            -- Grab the verbatim text
            verb_grabber_out = lt.token.scan_argument(false)

            -- Restore the original catcode table
            pop_catcodes()

            -- Return to \lua{luatools.verbatim:grab_until}
            tex.quittoks()
        end,
    }

    -- The parameters for the delimited macro
    local params_tabtoks = { --- @type tab_toklist
        { lt.token.cmd["match"], hash_chrcode }, -- Ignore the \endlocalcontrol
        { lt.token.cmd["match"], hash_chrcode }, -- The verbatim text
    }

    -- The body of the delimited macro
    local body_tabtoks = lt.token:to_tab_toklist {
        inner_macro,
        lt.token.cached["{"],
        token_create_chrcmd(2, lt.token.cmd["out_param"]), -- #2
        lt.token.cached["}"],
    }


    --- @param  stopper_toks any_toklist The terminating tokens.
    --- @return string   -   The contents of the environment.
    function luatools.verbatim:grab_until(stopper_toks)
        self = self.self

        -- If we're inside a macro argument, then everything has already been
        -- tokenized, so how we grab the text depends on where we are.
        local outer_level = (status.input_ptr <= 2)

        if not outer_level then
            -- Everything is already tokenized, including the stopper tokens,
            -- so we need to tokenize them using the current catcodes.
            stopper_toks = self.token:to_tab_toklist(stopper_toks)
        end

        -- Save the current catcode table and switch to the verbatim one
        push_catcodes(verbatim_cctab)

        if outer_level then
            -- Nothing has been tokenized yet, so the stopper tokens need to
            -- have the verbatim catcodes.
            stopper_toks = self.token:to_tab_toklist(stopper_toks)
        end

        -- Define a new delimitted macro to grab the environment's contents
        local outer_macro = lt.macro:from_toklist(
            { name = "outer_verb_grabber" },
            merge(params_tabtoks, stopper_toks),
            body_tabtoks,
            "long_call"
        )

        -- Grab the contents
        verb_grabber_out = nil
        self.token:run { outer_macro } -- Runs the macro \tex{verb_grabber}

        if not verb_grabber_out then
            lt.msg:error("Failed to grab verbatim text.")
            return --- @diagnostic disable-line: missing-return-value
        end

        -- Post-process the grabbed text
        return verb_postprocess(verb_grabber_out, outer_level)
    end
end


--= \subsection{\typ{luatools.verbatim:grab_braced}}
--=
--= Grabs the contents of a braced group as a verbatim string.

--- @return string - The contents of the braced group.
function luatools.verbatim:grab_braced()
    self = self.self

    -- Get the current level
    local outer_level = (status.input_ptr <= 2)

    -- Grab the text
    push_catcodes(almost_verb_cctab)
    local text = self.token.scan_argument(false)
    pop_catcodes()

    -- Post-process the grabbed text
    return verb_postprocess(text, outer_level)
end


--= \subsection{\typ{luatools.verbatim:grab_any}}
--=
--= Picks up the first token and uses it to determine how to grab the rest of
--= the text.

--- @return string - The contents of the environment.
function luatools.verbatim:grab_any()
    self = self.self

    -- Get the first token
    local first = self.token.get_next()

    -- If the first token is an opening brace, then scan until a balanced
    -- closing brace is found.
    if first.command == lt.token.cmd["left_brace"] then
        self.token:push { first }
        return self.verbatim:grab_braced()

    -- If the first token is a control sequence, then scan until we find the
    -- control sequence again.
    elseif first.csname then
        return self.verbatim:grab_until("\\" .. first.csname)

    -- If the first token is a character, then scan until the same character is
    -- found.
    elseif first.command >= lt.token.cmd["min_char_code"] and
           first.command <= lt.token.cmd["max_char_code"]
    then
        return self.verbatim:grab_until(utf_char(first.mode))

    -- Otherwise, scan for this exact token.
    else
        return self.verbatim:grab_until(first)
    end
end


--=--------------------
--= \section{Nodes} ---
--=--------------------
--=
--= Here, we define some functions for working with \LuaTeX{} nodes.

--- @alias node luatex.node
--- @alias user_whatsit luatex.node.whatsit.user_defined
--- @alias list_node luatex.node.list

--- @class luatools.node: _lt_base A table containing node functions.
--- @field self     luatools       The module root.
--- @field [string] fun(...):...   A wrapper around the \LuaTeX{} \typ{node}
---                                functions.
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

--- @param  node        node   The node to check.
--- @param  wanted_type string The type or subtype to check against.
--- @return boolean     -      Whether the node is of the specified type or
---                            subtype.
function luatools.node:is(node, wanted_type)
    self = self.self

    local given_type, given_subtype = self.node:type(node)

    return (wanted_type == given_type) or (wanted_type == given_subtype)
end


--= \subsection{\typ{luatools.node:has}}
--=
--= Checks if a node has the specified field.

--- @param  node  node   The node to check.
--- @param  field string The field to check for.
--- @return boolean -    Whether the node has the specified field.
function luatools.node:has(node, field)
    return node[field] ~= nil
end


--= \subsection{\typ{luatools.node:attr}}
--=
--= Gets the value of an attribute on a node.

--- @param  node    node           The node to get the attribute from.
--- @param  attr    string|integer The attribute to get.
--- @return integer -              The value of the attribute.
function luatools.node:attr(node, attr)
    self = self.self

    if type(attr) == "string" then
        local tok = self.tex:_get_token(attr)
        attr = tok.index
    end

    return self.node.get_attribute(node, attr)
end


--= \subsection{\typ{luatools.node:query}}
--=
--= Checks to see if a node matches the given query. Queries of the same type
--= are treated as an OR, while queries of different types are treated as an
--= AND.

--- @class (exact) _node_query   The query to check for.
--- @field node  node            The node to check.
--- @field is?   string|string[] The types of the node to check for.
--- @field has?  string|string[] The fields of the node to check for.
--- @field attr? string|string[] The attributes of the node to check for.

--= Makes sure that the query is in table form.
--- @param  query?   string|string[] The current query
--- @return string[] -               The fixed query
local function query_to_table(query)
    if type(query) == "nil" then
        return {}
    elseif type(query) == "string" then
        return { query }
    else
        return query
    end
end


--- @param  criteria _node_query The query to check for.
--- @return boolean  -           Whether the node matches the query.
function luatools.node:query(criteria)
    self = self.self

    local node = criteria.node

    -- Make sure that every entry of the table is a table
    criteria.is   = query_to_table(criteria.is  )
    criteria.has  = query_to_table(criteria.has )
    criteria.attr = query_to_table(criteria.attr)

    -- Loop over every query type in the \typ{query} table
    for _, query_name in pairs { "is", "has", "attr" } do
        local result       = true -- Empty queries are always true
        local query_values = criteria[query_name]

        -- Check to see if any of the queries return true
        for _, query_value in pairs(query_values) do
            result = self.node[query_name](self.node, node, query_value)
            if result then
                break
            end
        end

        if not result then
            return false
        end
    end

    return true
end


--= \subsection{\typ{luatools.node:new}}
--=
--= Creates a new node with the specified parameters.

--- @class (exact) _node_new The node creation parameters.
--- @field type     string   The type of the new node.
--- @field subtype? string   The subtype of the new node.
--- @field [string] any      Any additional fields to set on the new node.

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


--= \subsection{\typ{luatools.node:free_recursive}}
--=
--= Frees a node and all of its children.

local _free_recursive = luatex_lmtx(node.flush_node, node.flushnode)

--- @param  node node The node to free.
--- @return nil  -    -
function luatools.node:free_recursive(node)
    _free_recursive(node)
end


--= \subsection{\typ{luatools.node:free_following}}
--=
--= Frees a node and all of the nodes that follow it.

local _free_following = luatex_lmtx(node.flush_list, node.flushlist)

--- @param  head node The head of the node list to free.
--- @return nil  -    -
function luatools.node:free_following(head)
    _free_following(head)
end


--= \subsection{\typ{luatools.node:free_only}}
--=
--= Frees a node, but not its children.

--- @param  node  list_node The node to free.
--- @return node list       The head of the freed node's list.
--- @overload fun(node: node): nil
function luatools.node:free_only(node)
    local list = node.list
    if list then
        node.list = nil
    end

    _free_recursive(node)

    return list
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

    -- Free `find`
    self.node:free_recursive(find)

    return head
end


--= \subsection{\typ{luatools.node:to_str}}
--=
--= Converts the contents of a node list to a string.

local font_to_unicode = lt.util:memoized(function(font_id)
    local font = font.getfont(font_id)

    return lt.util:memoized(function(char)
        local codes = font.characters[char].unicode
        if type(codes) == "table" then
            return utf_char(unpack(codes))
        elseif type(codes) == "number" then
            return utf_char(codes)
        else
            return utf_char(char)
        end
    end)
end)

--- @param  head node The head of the list to convert.
--- @return string -  The contents of the list as a string.
function luatools.node:to_str(head)
    self = self.self

    local chars = {}

    for n in self.node.traverse(head) do
        if self.node:is(n, "glyph") then
            insert(chars, font_to_unicode[n.font][n.char])
        elseif self.node:is(n, "glue") then
            insert(chars, " ")
        elseif n.list or n.replace then
            insert(chars, self.node:to_str(n.list or n.replace))
        end
    end

    return concat(chars)
end


--= \subsection{\typ{luatools.node:from_str}}
--=
--= Converts a string into a node list.

--- @param  text     string               The string to convert.
--- @param  catcodes "verbatim"|"current" Whether to convert the characters
---                                       literally or to parse them using the
---                                       current \TeX{} catcodes.
--- @return node     head                 The head of the new node list.
function luatools.node:from_str(text, catcodes)
    self = self.self

    -- Choose the appropriate catcode table and box command
    local cctab, hbox  --- @type integer, user_tok
    if catcodes == "verbatim" then
        hbox  = self.token.cached["hpack"]
        cctab = verbatim_cctab
    elseif catcodes == "current" then
        hbox  = self.token.cached["hbox"]
        cctab = self.tex.catcodetable --[[@as integer]]
    else
        lt.msg:error("Invalid catcodes: " .. catcodes)
    end

    -- Move the string into a token register
    local register_csname = self.tex:new_register {
        name = "node_from_str_tmp",
        type = "toks",
    }
    tex.scantoks(register_csname, cctab, text)

    -- Push the tokens into \TeX{} and grab the resulting box
    local out_node --- @type list_node
    self.token._run(function()
        self.token:push {
            hbox,
            self.token.cached["{"],
            self.token.cached["the"],
            self.token.cached[register_csname],
            self.token.cached["}"],
        }
        out_node = self.token.scan_list()
    end)

    -- Return the inner list
    return self.node:free_only(out_node)
end


--= \subsection{\typ{luatools.node:get_next}}
--=
--= Gets the next node in a list that matches the specified criteria.

--- @class (exact) _node_get_next: _node_query The criteria to search for.
--- @field direction? "forward"|"backward"     The direction to search in.

--- @param  criteria _node_get_next The criteria to search for.
--- @return node? -                 The first node that matches the criteria.
function luatools.node:get_next(criteria)
    self = self.self

    local head      = criteria.node
    local direction = criteria.direction or "forward"

    if direction == "forward" then
        -- Use the standard traverser, but stop at the first match
        for n in self.node.traverse(head) do
            criteria.node = n
            if self.node:query(criteria) then
                return n
            end
        end
    elseif direction == "backward" and self.fmt.lmtx then
        -- LuaMetaTeX has a builtin reverse traversal function
        for n in self.node.traverse(head, true) do
            if self.node:query(criteria) then
                return n
            end
        end
    elseif direction == "backward" and not self.fmt.lmtx then
        -- For LuaTeX, we need to manually traverse the list in reverse
        local n = head
        while n do
            if self.node:query(criteria) then
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

--- @class (exact) _node_map: _node_query The parameters for mapping over a
---                                       list of nodes.
--- @field func (fun(n:node):...:node)|(fun(n:node):boolean?)
---             The function to apply to each node.

--- @param  params _node_map The parameters for mapping over the list.
--- @return node   head      The new head for the list.
function luatools.node:map(params)
    self = self.self

    local head = params.node

    -- Save all the nodes in a table so that the mapping function can safely
    -- modify the `next` pointers without breaking the traversal.
    local given_nodes = {} --- @type node[]
    for n in self.node.traverse(head) do
        insert(given_nodes, n)
    end

    -- Apply the function to each node
    local out_nodes = {} --- @type node[]
    for _, n in ipairs(given_nodes) do
        params.node = n
        if self.node:query(params) then
            local out = { params.func(n) }
            local first = out[1]
            if first == nil or first == true then
                -- If the function returns \lua{nil} or \lua{true}, then we
                -- add the node to the list as-is.
                insert(out_nodes, n)
            elseif first == false then
                -- If the function returns \lua{false}, then we skip the node.
            else
                -- Otherwise, we add the returned nodes to the list.
                append(out_nodes, out)
            end
        else
            insert(out_nodes, n)
        end
    end

    return self.node:join(out_nodes)
end


--= \subsection{\typ{luatools.node:map_recurse}}
--=
--= Maps a function over a list of nodes and through all of their nested lists.

--- @param  params _node_map The parameters for mapping over the list.
--- @return node   head      The new head for the list.
function luatools.node:map_recurse(params)
    self = self.self

    local head = params.node
    local func = params.func

    head = self.node:map {
        node = head,
        func = function(n)
            local out = n  --- @type node|boolean?

            params.node = n
            if self.node:query(params) then
                out = func(n)
            end

            if n.list then
                --- @cast n list_node
                params.node = n.list
                n.list = self.node:map_recurse(params)
            end

            return out
        end
    }

    return head
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
