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

package de.markusbordihn.easynpc.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.debug.DebugManager;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;

public class DebugCommand implements Command<CommandSourceStack> {

  private static final DebugCommand command = new DebugCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("debug")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(Commands.argument("enable", BoolArgumentType.bool()).executes(command));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    final boolean enable = BoolArgumentType.getBool(context, "enable");
    CommandSourceStack commandSource = context.getSource();
    if (enable) {
      commandSource.sendSuccess(
          Component.literal(
                  "► Enable debug for "
                      + Constants.MOD_NAME
                      + ", please check debug.log for the full output.")
              .withStyle(ChatFormatting.GREEN),
          false);
      commandSource.sendSuccess(
          Component.literal(
                  "> Use '/" + Constants.MOD_COMMAND + " debug false' to disable the debug!")
              .withStyle(ChatFormatting.WHITE),
          false);
    } else {
      commandSource.sendSuccess(
          Component.literal("■ Disable debug for " + Constants.MOD_NAME + "!")
              .withStyle(ChatFormatting.RED),
          false);
      commandSource.sendSuccess(
          Component.literal("> Please check the latest.log and/or debug.log for the full output.")
              .withStyle(ChatFormatting.WHITE),
          false);
    }
    DebugManager.enableDebugLevel(enable);
    return 0;
  }
}
