package de.markusbordihn.easynpc.server.commands.objectives;

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class AttackObjective extends Command {

  private static final String OBJECTIVE_NAME = "attack";

  private AttackObjective() {}

  public static ArgumentBuilder<CommandSourceStack, ?> registerRemove() {
    return Commands.literal(OBJECTIVE_NAME)
        .executes(
            context ->
                remove(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARGUMENT)));
  }

  public static ArgumentBuilder<CommandSourceStack, ?> registerList() {
    return Commands.literal(OBJECTIVE_NAME)
        .executes(
            context ->
                list(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARGUMENT)));
  }

  public static ArgumentBuilder<CommandSourceStack, ?> registerSet() {
    return Commands.literal(OBJECTIVE_NAME)
        .executes(
            context ->
                set(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARGUMENT)));
  }

  public static int remove(CommandSourceStack context, EasyNPC<?> easyNPC) {
    log.info("AttackObjective.remove {}", easyNPC);
    return Command.SINGLE_SUCCESS;
  }

  public static int list(CommandSourceStack context, EasyNPC<?> easyNPC) {
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null || !objectiveData.hasObjectives()) {
      return sendFailureMessageNoObjectiveData(context, easyNPC);
    }
    log.info("AttackObjective.list {}", easyNPC);
    return Command.SINGLE_SUCCESS;
  }

  public static int set(CommandSourceStack context, EasyNPC<?> easyNPC) {
    log.info("AttackObjective.set {}", easyNPC);
    return Command.SINGLE_SUCCESS;
  }
}
