-- Sanna Neovim Plugin
-- Provides LSP integration and commands for the Sanna specification language

local M = {}

-- Default configuration
M.config = {
    -- Path to sanna executable (nil = use PATH)
    server_path = nil,
    -- LSP settings
    lsp = {
        -- Auto-start LSP when opening .sanna files
        autostart = true,
        -- Enable diagnostics
        diagnostics = true,
        -- Enable hover
        hover = true,
        -- Enable completion
        completion = true,
        -- Enable go-to-definition
        definition = true,
    },
}

-- Setup function
function M.setup(opts)
    M.config = vim.tbl_deep_extend("force", M.config, opts or {})

    -- Register filetype
    vim.filetype.add({
        extension = {
            sanna = "sanna",
        },
    })

    -- Setup LSP if nvim-lspconfig is available
    local ok, lspconfig = pcall(require, "lspconfig")
    if ok then
        M.setup_lsp(lspconfig)
    end

    -- Register commands
    M.register_commands()
end

-- Setup LSP configuration
function M.setup_lsp(lspconfig)
    local configs = require("lspconfig.configs")

    -- Only add if not already defined
    if not configs.sanna then
        configs.sanna = {
            default_config = {
                cmd = { M.config.server_path or "sanna", "lsp" },
                filetypes = { "sanna" },
                root_dir = function(fname)
                    return lspconfig.util.find_git_ancestor(fname)
                        or lspconfig.util.path.dirname(fname)
                end,
                single_file_support = true,
                settings = {},
            },
        }
    end

    -- Setup with default or custom on_attach
    lspconfig.sanna.setup({
        autostart = M.config.lsp.autostart,
        capabilities = M.get_capabilities(),
        on_attach = M.on_attach,
    })
end

-- Get LSP capabilities
function M.get_capabilities()
    local capabilities = vim.lsp.protocol.make_client_capabilities()

    -- Add completion capabilities if nvim-cmp is available
    local ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
    if ok then
        capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
    end

    return capabilities
end

-- LSP on_attach callback
function M.on_attach(client, bufnr)
    local opts = { buffer = bufnr, silent = true }

    -- Keymaps
    if M.config.lsp.hover then
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    end

    if M.config.lsp.definition then
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    end

    if M.config.lsp.completion then
        vim.keymap.set("i", "<C-Space>", vim.lsp.buf.completion, opts)
    end

    -- Sanna-specific keymaps
    vim.keymap.set("n", "<leader>sv", function()
        M.verify()
    end, vim.tbl_extend("force", opts, { desc = "Sanna: Verify" }))

    vim.keymap.set("n", "<leader>sa", function()
        M.approve()
    end, vim.tbl_extend("force", opts, { desc = "Sanna: Approve" }))

    vim.keymap.set("n", "<leader>sr", function()
        M.request_review()
    end, vim.tbl_extend("force", opts, { desc = "Sanna: Request Review" }))

    vim.keymap.set("n", "<leader>st", function()
        M.trust_score()
    end, vim.tbl_extend("force", opts, { desc = "Sanna: Trust Score" }))
end

-- Register user commands
function M.register_commands()
    vim.api.nvim_create_user_command("SannaVerify", function()
        M.verify()
    end, { desc = "Verify current Sanna specification" })

    vim.api.nvim_create_user_command("SannaApprove", function()
        M.approve()
    end, { desc = "Approve current Sanna specification" })

    vim.api.nvim_create_user_command("SannaReview", function()
        M.request_review()
    end, { desc = "Request review for current Sanna specification" })

    vim.api.nvim_create_user_command("SannaTrust", function()
        M.trust_score()
    end, { desc = "Calculate trust score for current Sanna specification" })
end

-- Execute LSP command
function M.execute_command(command)
    local uri = vim.uri_from_bufnr(0)
    local params = {
        command = command,
        arguments = { uri },
    }

    vim.lsp.buf.execute_command(params)
end

-- Verify specification
function M.verify()
    M.execute_command("sanna.verify")
    vim.notify("Verifying specification...", vim.log.levels.INFO)
end

-- Approve specification
function M.approve()
    M.execute_command("sanna.approve")
    vim.notify("Specification approved", vim.log.levels.INFO)
end

-- Request review
function M.request_review()
    M.execute_command("sanna.review")
    vim.notify("Review requested", vim.log.levels.INFO)
end

-- Calculate trust score
function M.trust_score()
    M.execute_command("sanna.trust")
end

return M
