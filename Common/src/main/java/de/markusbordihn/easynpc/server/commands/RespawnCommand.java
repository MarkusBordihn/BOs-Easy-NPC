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

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.RespawnHandler;
import java.util.Collection;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class RespawnCommand extends Command {

  private RespawnCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("respawn")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument("targets", EasyNPCArgument.npc())
                .executes(
                    context ->
                        respawn(
                            context.getSource(),
                            EasyNPCArgument.getEntitiesWithAccess(context, "targets"))));
  }

  private static int respawn(
      CommandSourceStack context, Collection<? extends EasyNPC<?>> easyNPCs) {

    int respawnedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getUUID();
      if (AccessManager.hasAccess(context, uuid)) {
        if (RespawnHandler.respawnNPC(easyNPC, context.getLevel())) {
          respawnedEntities++;
        }
      } else {
        sendFailureMessage(context, "You are not allowed to respawn the Easy NPC " + uuid + " !");
      }
    }

    if (respawnedEntities == 1) {
      EasyNPC<?> easyNPC = easyNPCs.iterator().next();
      return sendSuccessMessage(
          context, "Respawn Easy NPC " + easyNPC.getEntity().getDisplayName().getString() + " !");
    } else if (respawnedEntities > 1) {
      return sendSuccessMessage(
          context,
          "Respawned " + respawnedEntities + " of " + easyNPCs.size() + " selected Easy NPCs!");
    }

    return sendFailureMessage(context, "Nothing to respawn!");
  }
}
