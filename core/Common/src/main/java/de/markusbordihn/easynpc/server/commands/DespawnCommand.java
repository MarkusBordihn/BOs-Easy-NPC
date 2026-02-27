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

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Arrays;
import java.util.Collection;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;

public class DespawnCommand extends Command {

  private static final String REASON_ARG = "reason";

  private DespawnCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("despawn")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc())
                .executes(
                    context ->
                        despawn(
                            context.getSource(),
                            EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG),
                            NPCRemovalReason.UNLOADED_BY_PLAYER))
                .then(
                    Commands.argument(REASON_ARG, StringArgumentType.word())
                        .suggests(
                            (context, builder) ->
                                SharedSuggestionProvider.suggest(
                                    Arrays.stream(NPCRemovalReason.values())
                                        .filter(r -> r != NPCRemovalReason.NONE)
                                        .map(NPCRemovalReason::name),
                                    builder))
                        .executes(
                            context ->
                                despawn(
                                    context.getSource(),
                                    EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG),
                                    NPCRemovalReason.fromString(
                                        StringArgumentType.getString(context, REASON_ARG))))));
  }

  private static int despawn(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      NPCRemovalReason reason) {

    int despawnedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getEntityUUID();
      if (AccessManager.hasAccess(context, uuid)) {
        if (EasyNPCEntityHandler.despawn(easyNPC, reason)) {
          despawnedEntities++;
        }
      } else {
        sendFailureMessage(context, "You are not allowed to despawn the Easy NPC " + uuid + " !");
      }
    }

    if (despawnedEntities == 1) {
      EasyNPC<?> easyNPC = easyNPCs.iterator().next();
      return sendSuccessMessage(
          context, "Despawned Easy NPC " + easyNPC.getEntity().getDisplayName().getString() + " !");
    } else if (despawnedEntities > 1) {
      return sendSuccessMessage(
          context,
          "Despawned " + despawnedEntities + " of " + easyNPCs.size() + " selected Easy NPCs!");
    }

    return sendFailureMessage(context, "Nothing to despawn!");
  }
}
