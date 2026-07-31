package de.markusbordihn.easynpc.server.commands.objectives;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;

public class AttackObjective extends Command {

  private static final String OBJECTIVE_NAME = "attack";
  private static final String TARGET_ARGUMENT = "target";
  private static final String ATTACK_TYPE_PREFIX = "ATTACK_";

  private static final Map<ObjectiveType, ProtectionObjective> PROTECTION_OBJECTIVES =
      new LinkedHashMap<>();
  private static final SuggestionProvider<CommandSourceStack> SUGGEST_ATTACK_TARGETS =
      (context, builder) ->
          SharedSuggestionProvider.suggest(
              Arrays.stream(ObjectiveType.values())
                  .filter(BuiltInObjectiveFactories::isTargetObjective)
                  .map(AttackObjective::getFriendlyTargetName)
                  .collect(Collectors.toList()),
              builder);

  static {
    PROTECTION_OBJECTIVES.put(
        ObjectiveType.OWNER_HURT_BY_TARGET,
        new ProtectionObjective("protect_owner", "Protect Owner"));
    PROTECTION_OBJECTIVES.put(
        ObjectiveType.HURT_BY_TARGET, new ProtectionObjective("defend_self", "Defend Self"));
    PROTECTION_OBJECTIVES.put(
        ObjectiveType.FACTION_HURT_BY_TARGET,
        new ProtectionObjective("defend_faction", "Defend Faction"));
  }

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
      if (BuiltInObjectiveFactories.isTargetObjective(objectiveType)) {
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
      if (BuiltInObjectiveFactories.isTargetObjective(objectiveType)
          && !PROTECTION_OBJECTIVES.containsKey(objectiveType)) {
        objectiveData
            .getObjectiveEntry(objectiveType)
            .ifPresent(
                objective ->
                    sendSuccessMessage(context, "> " + getFriendlyTargetName(objective.getType())));
      }
    }

    // List protection targets
    for (Map.Entry<ObjectiveType, ProtectionObjective> protectionEntry :
        PROTECTION_OBJECTIVES.entrySet()) {
      objectiveData
          .getObjectiveEntry(protectionEntry.getKey())
          .ifPresent(
              objective ->
                  sendSuccessMessage(
                      context,
                      "> "
                          + protectionEntry.getValue().friendlyName()
                          + " ("
                          + protectionEntry.getValue().label()
                          + ")"));
    }

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
      if (BuiltInObjectiveFactories.isTargetObjective(objectiveType)) {
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

    ProtectionObjective protectionObjective = PROTECTION_OBJECTIVES.get(objectiveType);
    if (protectionObjective != null) {
      return protectionObjective.friendlyName();
    }

    if (objectiveType.name().startsWith(ATTACK_TYPE_PREFIX)) {
      return objectiveType.name().substring(ATTACK_TYPE_PREFIX.length()).toLowerCase(Locale.ROOT);
    }

    return objectiveType.getObjectiveName();
  }

  private static ObjectiveType getObjectiveTypeFromFriendlyName(String targetName) {
    if (targetName == null || targetName.isEmpty()) {
      return null;
    }

    String friendlyTargetName = targetName.toLowerCase(Locale.ROOT);
    for (Map.Entry<ObjectiveType, ProtectionObjective> protectionEntry :
        PROTECTION_OBJECTIVES.entrySet()) {
      if (protectionEntry.getValue().friendlyName().equals(friendlyTargetName)) {
        return protectionEntry.getKey();
      }
    }

    String attackTypeName = ATTACK_TYPE_PREFIX + friendlyTargetName.toUpperCase(Locale.ROOT);
    try {
      return ObjectiveType.valueOf(attackTypeName);
    } catch (IllegalArgumentException e) {
      return null;
    }
  }

  private record ProtectionObjective(String friendlyName, String label) {}
}
