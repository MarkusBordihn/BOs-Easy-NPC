/*
 * Copyright 2026 Markus Bordihn
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

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.api.handler.EasyNPCPauseHandler;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Collection;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.permissions.Permissions;

public class PauseCommand extends Command {

  private PauseCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("pause")
        .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
        .then(
            Commands.literal("all")
                .then(
                    Commands.argument(ENABLE_ARG, BoolArgumentType.bool())
                        .executes(
                            context ->
                                pauseAll(
                                    context.getSource(),
                                    BoolArgumentType.getBool(context, ENABLE_ARG)))))
        .then(
            Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc())
                .executes(
                    context ->
                        pause(
                            context.getSource(),
                            EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG),
                            true))
                .then(
                    Commands.argument(ENABLE_ARG, BoolArgumentType.bool())
                        .executes(
                            context ->
                                pause(
                                    context.getSource(),
                                    EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG),
                                    BoolArgumentType.getBool(context, ENABLE_ARG)))));
  }

  private static int pause(
      CommandSourceStack context, Collection<? extends EasyNPC<?>> easyNPCs, boolean paused) {
    int changedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      if (paused ? EasyNPCPauseHandler.pause(easyNPC) : EasyNPCPauseHandler.resume(easyNPC)) {
        changedEntities++;
      }
    }

    if (changedEntities == 0) {
      return sendFailureMessage(context, "Nothing to " + (paused ? "pause" : "resume") + "!");
    }

    return sendSuccessMessage(
        context,
        (paused ? "Paused " : "Resumed ")
            + changedEntities
            + " of "
            + easyNPCs.size()
            + " Easy NPCs!");
  }

  private static int pauseAll(CommandSourceStack context, boolean paused) {
    EasyNPCPauseHandler.setGlobalPause(paused);
    return sendSuccessMessage(
        context,
        paused
            ? "Paused all Easy NPCs, including the ones which are loaded later!"
            : "Resumed all Easy NPCs, except the ones which are paused individually!");
  }
}
