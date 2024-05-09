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
import java.util.Collection;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class DeleteCommand extends Command {

  private DeleteCommand() {
  }

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("delete")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument("targets", new EasyNPCArgument())
                .executes(
                    context ->
                        delete(
                            context.getSource(),
                            EasyNPCArgument.getEntitiesWithAccess(context, "targets"))));
  }

  private static int delete(CommandSourceStack context, Collection<? extends EasyNPC<?>> easyNPCs) {

    int deletedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getUUID();
      if (AccessManager.hasAccess(context, uuid)) {
        easyNPC.getEntity().discard();
        deletedEntities++;
      } else {
        sendFailureMessage(context, "You are not allowed to delete the Easy NPC " + uuid + " !");
      }
    }

    if (deletedEntities == 1) {
      EasyNPC<?> easyNPC = easyNPCs.iterator().next();
      return sendSuccessMessage(
          context, "Deleted Easy NPC " + easyNPC.getEntity().getDisplayName().getString() + " !");
    } else if (deletedEntities > 1) {
      return sendSuccessMessage(
          context,
          "Deleted " + deletedEntities + " of " + easyNPCs.size() + " selected Easy NPCs!");
    }

    return sendFailureMessage(context, "Nothing to delete!");
  }
}
