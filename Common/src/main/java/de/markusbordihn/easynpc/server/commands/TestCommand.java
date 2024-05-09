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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.suggestion.EasyNPCSuggestions;
import de.markusbordihn.easynpc.menu.MenuHandlerInterface;
import de.markusbordihn.easynpc.menu.MenuManager;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TestCommand {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TestCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("test")
        .requires(commandSource -> commandSource.hasPermission(Commands.LEVEL_ADMINS))
        .then(
            Commands.literal("screen")
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(EasyNPCSuggestions::suggestUUID)
                        .executes(
                            commandContext ->
                                testScreen(
                                    commandContext.getSource(),
                                    UuidArgument.getUuid(commandContext, "uuid")))));
  }

  public static int testScreen(CommandSourceStack context, UUID npcUUID)
      throws CommandSyntaxException {
    context.sendSuccess(new TextComponent("Test screen command executed!"), true);
    ServerPlayer serverPlayer = context.getPlayerOrException();
    MenuHandlerInterface menuHandler = MenuManager.getMenuHandler();
    menuHandler.openTestMenu(serverPlayer, npcUUID);

    return Command.SINGLE_SUCCESS;
  }
}
