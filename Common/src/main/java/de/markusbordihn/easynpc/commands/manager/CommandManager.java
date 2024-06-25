/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.commands.manager;

import com.mojang.brigadier.CommandDispatcher;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.server.commands.ConfigureCommand;
import de.markusbordihn.easynpc.server.commands.DebugCommand;
import de.markusbordihn.easynpc.server.commands.DeleteCommand;
import de.markusbordihn.easynpc.server.commands.DialogCommand;
import de.markusbordihn.easynpc.server.commands.NavigationCommand;
import de.markusbordihn.easynpc.server.commands.OwnerCommand;
import de.markusbordihn.easynpc.server.commands.PresetCommand;
import de.markusbordihn.easynpc.server.commands.RenderCommand;
import de.markusbordihn.easynpc.server.commands.SkinCommand;
import de.markusbordihn.easynpc.server.commands.SoundCommand;
import de.markusbordihn.easynpc.server.commands.TradingCommand;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CommandManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CommandManager() {}

  public static void registerCommands(CommandDispatcher<CommandSourceStack> commandDispatcher) {
    log.info(
        "{} /{} commands for {} ...",
        Constants.LOG_REGISTER_PREFIX,
        Constants.MOD_COMMAND,
        Constants.MOD_NAME);
    commandDispatcher.register(
        Commands.literal(Constants.MOD_COMMAND)
            // @formatter:off
            .then(ConfigureCommand.register())
            .then(DebugCommand.register())
            .then(DeleteCommand.register())
            .then(DialogCommand.register())
            .then(NavigationCommand.register())
            .then(OwnerCommand.register())
            .then(PresetCommand.register())
            .then(RenderCommand.register())
            .then(SkinCommand.register())
            .then(SoundCommand.register())
            .then(TradingCommand.register())
        // @formatter:on
        );
  }
}
