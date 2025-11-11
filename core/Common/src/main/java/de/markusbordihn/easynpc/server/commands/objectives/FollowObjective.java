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

package de.markusbordihn.easynpc.server.commands.objectives;

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveGroup;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;

public class FollowObjective extends Command {

  private static final String OBJECTIVE_NAME = "follow";

  private FollowObjective() {}

  public static ArgumentBuilder<CommandSourceStack, ?> registerSet() {
    return Commands.literal(OBJECTIVE_NAME)
        .then(
            Commands.literal(ObjectiveType.FOLLOW_OWNER.getFriendlyName())
                .executes(
                    context ->
                        setFollowOwner(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG))))
        .then(
            Commands.literal(ObjectiveType.FOLLOW_PLAYER.getFriendlyName())
                .then(
                    Commands.argument("player", EntityArgument.player())
                        .executes(
                            context ->
                                setFollowPlayer(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    EntityArgument.getPlayer(context, "player")))))
        .then(
            Commands.literal(ObjectiveType.FOLLOW_ENTITY_BY_UUID.getFriendlyName())
                .then(
                    Commands.argument("entity", EntityArgument.entity())
                        .executes(
                            context ->
                                setFollowEntity(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    EntityArgument.getEntity(context, "entity")))));
  }

  public static ArgumentBuilder<CommandSourceStack, ?> registerList() {
    return Commands.literal(OBJECTIVE_NAME)
        .executes(
            context ->
                list(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)));
  }

  public static ArgumentBuilder<CommandSourceStack, ?> registerRemove() {
    return Commands.literal(OBJECTIVE_NAME)
        .then(
            Commands.literal(ObjectiveType.FOLLOW_OWNER.getFriendlyName())
                .executes(
                    context ->
                        removeFollowObjective(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            ObjectiveType.FOLLOW_OWNER)))
        .then(
            Commands.literal(ObjectiveType.FOLLOW_PLAYER.getFriendlyName())
                .executes(
                    context ->
                        removeFollowObjective(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            ObjectiveType.FOLLOW_PLAYER)))
        .then(
            Commands.literal(ObjectiveType.FOLLOW_ENTITY_BY_UUID.getFriendlyName())
                .executes(
                    context ->
                        removeFollowObjective(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            ObjectiveType.FOLLOW_ENTITY_BY_UUID)));
  }

  public static int removeFollowObjective(
      CommandSourceStack context, EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
    ObjectiveDataCapable<?> objectiveData = getFollowObjectiveData(context, easyNPC);
    if (objectiveData == null || objectiveType == null) {
      return Command.FAILURE;
    }

    if (!objectiveData.removeCustomObjective(objectiveType)) {
      return sendFailureMessage(
          context, "Error removing follow objective " + objectiveType + " for " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Removed follow objective " + objectiveType + " for " + easyNPC);
  }

  public static int list(CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = getFollowObjectiveData(context, easyNPC);
    if (objectiveData == null) {
      return Command.FAILURE;
    }

    // List follow objectives
    sendSuccessMessage(context, "Follow objectives for " + easyNPC + ":");
    objectiveData
        .getObjectiveEntry(ObjectiveType.FOLLOW_OWNER)
        .ifPresent(
            objective ->
                sendSuccessMessage(
                    context, "> " + objective.getType() + ": " + objective.getTargetOwnerUUID()));
    objectiveData
        .getObjectiveEntry(ObjectiveType.FOLLOW_PLAYER)
        .ifPresent(
            objective ->
                sendSuccessMessage(
                    context, "> " + objective.getType() + ": " + objective.getTargetPlayerName()));
    objectiveData
        .getObjectiveEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID)
        .ifPresent(
            objective ->
                sendSuccessMessage(
                    context, "> " + objective.getType() + ": " + objective.getTargetEntityUUID()));

    return Command.SINGLE_SUCCESS;
  }

  public static int setFollowOwner(CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = getObjectiveData(context, easyNPC);
    if (objectiveData == null) {
      return Command.FAILURE;
    }

    // Get owner data
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null || !ownerData.hasNPCOwner() || ownerData.getOwnerUUID() == null) {
      return sendFailureMessageNoOwnerData(context, easyNPC);
    }

    // Add or update follow owner objective
    ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
    objectiveDataEntry.setTargetOwnerUUID(ownerData.getOwnerUUID());
    if (!objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      return sendFailureMessage(context, "Error adding or updating follow owner objective!");
    }

    return sendSuccessMessage(context, "Follow owner objective added for " + easyNPC);
  }

  public static int setFollowPlayer(
      CommandSourceStack context, EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    ObjectiveDataCapable<?> objectiveData = getObjectiveData(context, easyNPC);
    if (objectiveData == null || serverPlayer == null) {
      return Command.FAILURE;
    }

    // Add or update follow player objective
    ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER);
    objectiveDataEntry.setTargetPlayerName(serverPlayer.getName().getString());
    if (!objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      return sendFailureMessage(context, "Error adding or updating follow player objective!");
    }

    return sendSuccessMessage(
        context,
        "Follow player " + serverPlayer.getName().getString() + " objective added for " + easyNPC);
  }

  public static int setFollowEntity(CommandSourceStack context, EasyNPC<?> easyNPC, Entity entity) {
    ObjectiveDataCapable<?> objectiveData = getObjectiveData(context, easyNPC);
    if (objectiveData == null || entity == null) {
      return Command.FAILURE;
    }

    // Avoid adding the same entity as target
    if (entity.getUUID().equals(easyNPC.getEntityUUID())) {
      return sendFailureMessage(context, "Error adding follow entity objective for itself!");
    }

    // Add or update follow player objective
    ObjectiveDataEntry objectiveDataEntry =
        new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
    objectiveDataEntry.setTargetEntityUUID(entity.getUUID());
    if (!objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      return sendFailureMessage(context, "Error adding or updating follow player objective!");
    }

    return sendSuccessMessage(
        context,
        "Follow entity " + entity.getName().getString() + " objective added for " + easyNPC);
  }

  private static ObjectiveDataCapable<?> getObjectiveData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null || !objectiveData.hasObjectives()) {
      sendFailureMessageNoObjectiveData(context, easyNPC);
      return null;
    }
    return objectiveData;
  }

  private static ObjectiveDataCapable<?> getFollowObjectiveData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = getObjectiveData(context, easyNPC);
    if (objectiveData == null) {
      return null;
    }

    if (!objectiveData.hasObjectives(ObjectiveGroup.FOLLOW)) {
      sendFailureMessageNoObjectiveData(context, easyNPC, "follow");
      return null;
    }

    return objectiveData;
  }
}
