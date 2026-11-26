package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.server.commands.ModelPartCommand.ModelPartChange;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class ScaleCommand extends Command {

  private static final ModelPartChange SCALE_CHANGE =
      new ModelPartChange(
          "scale",
          "scaled",
          (modelData, modelPartType, x, y, z) ->
              modelData.setModelPartScale(modelPartType, new CustomScale(x, y, z)));

  private ScaleCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("scale")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc())
                .then(
                    ModelPartCommand.modelPartArgument()
                        .then(
                            Commands.argument(SCALE_ARG, FloatArgumentType.floatArg(0.0F, 10.0F))
                                .executes(context -> {
                                  float scale = FloatArgumentType.getFloat(context, SCALE_ARG);
                                  return ModelPartCommand.changeModelPart(
                                      context, SCALE_CHANGE, scale, scale, scale);
                                }))
                        .then(ModelPartCommand.axisArguments(0.0F, 10.0F, SCALE_CHANGE))));
  }
}
