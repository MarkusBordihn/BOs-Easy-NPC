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
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.suggestion.DespawnedNPCSuggestions;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.world.phys.Vec3;

public class ReviveCommand extends Command {

  private static final String UUID_ARG = "uuid";

  private ReviveCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("revive")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument(UUID_ARG, StringArgumentType.string())
                .suggests(DespawnedNPCSuggestions::suggestRevivable)
                .executes(
                    context ->
                        revive(
                            context.getSource(),
                            StringArgumentType.getString(context, UUID_ARG),
                            null))
                .then(
                    Commands.argument(POSITION_ARG, Vec3Argument.vec3())
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
                        .executes(
                            context ->
                                revive(
                                    context.getSource(),
                                    StringArgumentType.getString(context, UUID_ARG),
                                    Vec3Argument.getVec3(context, POSITION_ARG)))));
  }

  private static int revive(CommandSourceStack context, String uuidString, Vec3 position) {
    UUID uuid = SpawnCommand.parseUUID(context, uuidString);
    if (uuid == null) {
      return FAILURE;
    }

    if (!SpawnCommand.hasAccessToStoredNPC(context, uuid)) {
      return sendFailureMessage(context, "You are not allowed to revive the Easy NPC " + uuid + " !");
    }

    Optional<String> reviveRejection = EasyNPCEntityHandler.getReviveRejection(uuid);
    if (reviveRejection.isPresent()) {
      return sendFailureMessage(
          context, "Cannot revive Easy NPC " + uuid + ": " + reviveRejection.get() + "!");
    }

    boolean revived;
    if (position != null) {
      revived = EasyNPCEntityHandler.revive(uuid, context.getLevel(), position);
    } else {
      revived = EasyNPCEntityHandler.revive(uuid, SpawnCommand.getStoredLevel(context, uuid));
    }
    if (!revived) {
      return sendFailureMessage(context, "Failed to revive Easy NPC " + uuid + " !");
    }

    return sendSuccessMessage(context, "Revived Easy NPC " + uuid + " !");
  }
}
