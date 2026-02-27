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
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class SpawnCommand extends Command {

  private static final String UUID_ARG = "uuid";
  private static final String POSITION_ARG = "position";

  private SpawnCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("spawn")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument(UUID_ARG, StringArgumentType.string())
                .suggests(DespawnedNPCSuggestions::suggest)
                .executes(
                    context ->
                        spawn(context.getSource(), StringArgumentType.getString(context, UUID_ARG)))
                .then(
                    Commands.argument(POSITION_ARG, Vec3Argument.vec3())
                        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
                        .executes(
                            context ->
                                spawnAtPosition(
                                    context.getSource(),
                                    StringArgumentType.getString(context, UUID_ARG),
                                    Vec3Argument.getVec3(context, POSITION_ARG)))));
  }

  private static int spawn(CommandSourceStack context, String uuidString) {
    UUID uuid = parseUUID(context, uuidString);
    if (uuid == null) return FAILURE;

    if (!hasAccessToStoredNPC(context, uuid)) {
      return sendFailureMessage(
          context, "You are not allowed to spawn the Easy NPC " + uuid + " !");
    }

    Optional<NPCEntityMetadata> meta = NPCEntityData.get().getMetadata(uuid);
    if (meta.isEmpty()) {
      return sendFailureMessage(context, "No saved data found for NPC " + uuid + " !");
    }

    ServerLevel serverLevel = context.getLevel();
    if (meta.get().hasDimension()) {
      ServerLevel targetLevel =
          context
              .getServer()
              .getLevel(
                  ResourceKey.create(
                      Registries.DIMENSION, ResourceLocation.parse(meta.get().dimension())));
      if (targetLevel != null) {
        serverLevel = targetLevel;
      }
    }

    if (EasyNPCEntityHandler.spawn(uuid, serverLevel)) {
      return sendSuccessMessage(context, "Spawned Easy NPC " + uuid + " !");
    }
    return sendFailureMessage(context, "Failed to spawn Easy NPC " + uuid + " !");
  }

  private static int spawnAtPosition(CommandSourceStack context, String uuidString, Vec3 position) {
    UUID uuid = parseUUID(context, uuidString);
    if (uuid == null) return FAILURE;

    if (!hasAccessToStoredNPC(context, uuid)) {
      return sendFailureMessage(
          context, "You are not allowed to spawn the Easy NPC " + uuid + " !");
    }

    if (EasyNPCEntityHandler.spawn(uuid, context.getLevel(), position)) {
      return sendSuccessMessage(
          context,
          "Spawned Easy NPC "
              + uuid
              + " at "
              + (int) position.x
              + " "
              + (int) position.y
              + " "
              + (int) position.z
              + " !");
    }
    return sendFailureMessage(context, "Failed to spawn Easy NPC " + uuid + " !");
  }

  private static UUID parseUUID(CommandSourceStack context, String uuidString) {
    try {
      return UUID.fromString(uuidString);
    } catch (IllegalArgumentException e) {
      sendFailureMessage(context, "Invalid UUID: " + uuidString);
      return null;
    }
  }

  private static boolean hasAccessToStoredNPC(CommandSourceStack context, UUID uuid) {
    if (context.hasPermission(Commands.LEVEL_GAMEMASTERS)) {
      return true;
    }
    try {
      ServerPlayer serverPlayer = context.getPlayerOrException();
      if (serverPlayer.isCreative()) {
        return true;
      }
      Optional<NPCEntityMetadata> meta = NPCEntityData.get().getMetadata(uuid);
      return meta.isPresent()
          && meta.get().hasOwner()
          && meta.get().ownerUUID().equals(serverPlayer.getUUID());
    } catch (Exception e) {
      return true;
    }
  }
}
