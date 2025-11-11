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

package de.markusbordihn.easynpc.configui.server.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.configui.menu.MenuManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;

public class ConfigureCommand extends Command {

  private ConfigureCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("configure")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                .executes(
                    context ->
                        configure(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG))));
  }

  private static int configure(CommandSourceStack context, EasyNPC<?> easyNPC)
      throws CommandSyntaxException {
    ServerPlayer serverPlayer = context.getPlayerOrException();

    // Verify configuration data for the EasyNPC.
    ConfigurationDataCapable<?> configurationData = easyNPC.getEasyNPCConfigurationData();
    if (configurationData == null) {
      return sendFailureMessage(context, "This EasyNPC does not support configuration!");
    }

    MenuManager.getMenuHandler()
        .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
    return Command.SINGLE_SUCCESS;
  }
}
