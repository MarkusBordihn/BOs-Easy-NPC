package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.server.commands.ModelPartCommand.ModelPartChange;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class PositionCommand extends Command {

  private static final ModelPartChange MOVE_CHANGE =
      new ModelPartChange(
          "move",
          "moved",
          (modelData, modelPartType, x, y, z) ->
              modelData.setModelPartPosition(modelPartType, new CustomPosition(x, y, z)));

  private PositionCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("position")
        .then(
            Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc())
                .then(
                    ModelPartCommand.modelPartArgument()
                        .then(ModelPartCommand.axisArguments(-3.0F, 3.0F, MOVE_CHANGE))));
  }
}
