package de.markusbordihn.easynpc.server.commands.objectives;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import java.util.Arrays;
import java.util.stream.Collectors;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;

public class AttackObjective extends Command {

  private static final String OBJECTIVE_NAME = "attack";
  private static final String TARGET_ARGUMENT = "target";

  private static final SuggestionProvider<CommandSourceStack> SUGGEST_ATTACK_TARGETS =
      (context, builder) ->
          SharedSuggestionProvider.suggest(
              Arrays.stream(ObjectiveType.values())
                  .filter(
                      objectiveType ->
                          objectiveType.name().startsWith("ATTACK_")
                              || objectiveType == ObjectiveType.HURT_BY_TARGET
                              || objectiveType == ObjectiveType.OWNER_HURT_BY_TARGET)
                  .map(AttackObjective::getFriendlyTargetName)
                  .collect(Collectors.toList()),
              builder);

  private AttackObjective() {}

  public static ArgumentBuilder<CommandSourceStack, ?> registerRemove() {
    return Commands.literal(OBJECTIVE_NAME)
        .then(
            Commands.literal("target")
                .then(
                    Commands.argument(TARGET_ARGUMENT, StringArgumentType.word())
                        .suggests(SUGGEST_ATTACK_TARGETS)
                        .executes(
                            context ->
                                removeAttackTargetObjective(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    StringArgumentType.getString(context, TARGET_ARGUMENT)))))
        .executes(
            context ->
                remove(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)));
  }

  public static ArgumentBuilder<CommandSourceStack, ?> registerList() {
    return Commands.literal(OBJECTIVE_NAME)
        .then(
            Commands.literal("target")
                .executes(
                    context ->
                        list(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG))))
        .executes(
            context ->
                list(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)));
  }

  public static ArgumentBuilder<CommandSourceStack, ?> registerSet() {
    return Commands.literal(OBJECTIVE_NAME)
        .then(
            Commands.literal("target")
                .then(
                    Commands.argument(TARGET_ARGUMENT, StringArgumentType.word())
                        .suggests(SUGGEST_ATTACK_TARGETS)
                        .executes(
                            context ->
                                setAttackTargetObjective(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    StringArgumentType.getString(context, TARGET_ARGUMENT)))))
        .executes(
            context ->
                set(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)));
  }

  public static int remove(CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null || !objectiveData.hasObjectives()) {
      return sendFailureMessageNoObjectiveData(context, easyNPC);
    }

    boolean removedAny = false;
    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      if (objectiveType.name().startsWith("ATTACK_")
          || objectiveType == ObjectiveType.HURT_BY_TARGET
          || objectiveType == ObjectiveType.OWNER_HURT_BY_TARGET) {
        if (objectiveData.removeCustomObjective(objectiveType)) {
          removedAny = true;
        }
      }
    }

    if (removedAny) {
      return sendSuccessMessage(context, "Removed all attack objectives for " + easyNPC);
    } else {
      return sendFailureMessage(context, "No attack objectives found for " + easyNPC);
    }
  }

  public static int removeAttackTargetObjective(
      CommandSourceStack context, EasyNPC<?> easyNPC, String targetName) {
    ObjectiveType objectiveType = getObjectiveTypeFromFriendlyName(targetName);
    if (objectiveType == null) {
      return sendFailureMessage(context, "Unknown attack target type: " + targetName);
    }

    ObjectiveDataCapable<?> objectiveData = getAttackObjectiveData(context, easyNPC);
    if (objectiveData == null) {
      return Command.FAILURE;
    }

    if (!objectiveData.removeCustomObjective(objectiveType)) {
      return sendFailureMessage(
          context, "Error removing attack target " + targetName + " for " + easyNPC);
    }

    return sendSuccessMessage(context, "Removed attack target " + targetName + " for " + easyNPC);
  }

  public static int list(CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null || !objectiveData.hasObjectives()) {
      return sendFailureMessageNoObjectiveData(context, easyNPC);
    }

    sendSuccessMessage(context, "Attack targets for " + easyNPC + ":");

    // List standard attack targets
    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      if (objectiveType.name().startsWith("ATTACK_")) {
        objectiveData
            .getObjectiveEntry(objectiveType)
            .ifPresent(
                objective ->
                    sendSuccessMessage(context, "> " + getFriendlyTargetName(objective.getType())));
      }
    }

    // List protection targets
    objectiveData
        .getObjectiveEntry(ObjectiveType.OWNER_HURT_BY_TARGET)
        .ifPresent(
            objective ->
                sendSuccessMessage(
                    context,
                    "> " + getFriendlyTargetName(objective.getType()) + " (Protect Owner)"));

    objectiveData
        .getObjectiveEntry(ObjectiveType.HURT_BY_TARGET)
        .ifPresent(
            objective ->
                sendSuccessMessage(
                    context, "> " + getFriendlyTargetName(objective.getType()) + " (Defend Self)"));

    return Command.SINGLE_SUCCESS;
  }

  public static int set(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return sendSuccessMessage(context, "Usage: /objective set attack target <target_type>");
  }

  public static int setAttackTargetObjective(
      CommandSourceStack context, EasyNPC<?> easyNPC, String targetName) {
    ObjectiveType objectiveType = getObjectiveTypeFromFriendlyName(targetName);
    if (objectiveType == null) {
      return sendFailureMessage(context, "Unknown attack target type: " + targetName);
    }

    ObjectiveDataCapable<?> objectiveData = getObjectiveData(context, easyNPC);
    if (objectiveData == null) {
      return Command.FAILURE;
    }

    ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(objectiveType);
    if (!objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      return sendFailureMessage(context, "Error adding or updating attack target!");
    }

    return sendSuccessMessage(context, "Attack target " + targetName + " added for " + easyNPC);
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

  private static ObjectiveDataCapable<?> getAttackObjectiveData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = getObjectiveData(context, easyNPC);
    if (objectiveData == null) {
      return null;
    }

    boolean hasAnyAttackObjective = false;
    for (ObjectiveType objectiveType : ObjectiveType.values()) {
      if (objectiveType.name().startsWith("ATTACK_")
          || objectiveType == ObjectiveType.HURT_BY_TARGET
          || objectiveType == ObjectiveType.OWNER_HURT_BY_TARGET) {
        if (objectiveData.hasObjective(objectiveType)) {
          hasAnyAttackObjective = true;
          break;
        }
      }
    }

    if (!hasAnyAttackObjective) {
      sendFailureMessageNoObjectiveData(context, easyNPC, "attack");
      return null;
    }

    return objectiveData;
  }

  private static String getFriendlyTargetName(ObjectiveType objectiveType) {
    if (objectiveType == null) {
      return "";
    }

    if (objectiveType == ObjectiveType.HURT_BY_TARGET) {
      return "defend_self";
    } else if (objectiveType == ObjectiveType.OWNER_HURT_BY_TARGET) {
      return "protect_owner";
    } else if (objectiveType.name().startsWith("ATTACK_")) {
      return objectiveType.name().substring(7).toLowerCase();
    }

    return objectiveType.name().toLowerCase();
  }

  private static ObjectiveType getObjectiveTypeFromFriendlyName(String targetName) {
    if (targetName == null || targetName.isEmpty()) {
      return null;
    }

    targetName = targetName.toLowerCase();

    if ("defend_self".equals(targetName)) {
      return ObjectiveType.HURT_BY_TARGET;
    } else if ("protect_owner".equals(targetName)) {
      return ObjectiveType.OWNER_HURT_BY_TARGET;
    } else {
      try {
        return ObjectiveType.valueOf("ATTACK_" + targetName.toUpperCase());
      } catch (IllegalArgumentException e) {
        return null;
      }
    }
  }
}
