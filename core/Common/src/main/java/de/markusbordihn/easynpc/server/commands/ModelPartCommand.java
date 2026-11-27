package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.Collection;
import java.util.Locale;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

final class ModelPartCommand extends Command {

  private ModelPartCommand() {}

  static RequiredArgumentBuilder<CommandSourceStack, String> modelPartArgument() {
    return Commands.argument(MODEL_PART_ARG, StringArgumentType.word())
        .suggests((context, builder) -> {
          EasyNPC<?> easyNPC = EasyNPCArgument.getEntityWithAccess(context, NPC_TARGETS_ARG);
          if (easyNPC != null) {
            ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
            builder.suggest(ModelPartType.ROOT.name().toLowerCase(Locale.ROOT));
            for (ModelPartType partType : modelData.getModelType().getModelParts()) {
              builder.suggest(partType.name().toLowerCase(Locale.ROOT));
            }
          }
          return builder.buildFuture();
        });
  }

  static RequiredArgumentBuilder<CommandSourceStack, Float> axisArguments(
      float minimum, float maximum, ModelPartChange modelPartChange) {
    return Commands.argument(X_ARG, FloatArgumentType.floatArg(minimum, maximum))
        .then(
            Commands.argument(Y_ARG, FloatArgumentType.floatArg(minimum, maximum))
                .then(
                    Commands.argument(Z_ARG, FloatArgumentType.floatArg(minimum, maximum))
                        .executes(context ->
                            changeModelPart(
                                context,
                                modelPartChange,
                                FloatArgumentType.getFloat(context, X_ARG),
                                FloatArgumentType.getFloat(context, Y_ARG),
                                FloatArgumentType.getFloat(context, Z_ARG)))));
  }

  static int changeModelPart(
      CommandContext<CommandSourceStack> context,
      ModelPartChange modelPartChange,
      float x,
      float y,
      float z)
      throws CommandSyntaxException {
    String partString = StringArgumentType.getString(context, MODEL_PART_ARG);
    ModelPartType modelPartType = ModelPartType.get(partString);
    CommandSourceStack source = context.getSource();
    if (modelPartType == ModelPartType.UNKNOWN) {
      return sendFailureMessage(source, "Invalid model part " + partString);
    }

    Collection<? extends EasyNPC<?>> easyNPCs =
        EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG);
    int changed = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getEntityUUID();
      if (AccessManager.hasAccess(source, uuid)) {
        ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
        if (!modelData.supportsModelPart(modelPartType)) {
          sendFailureMessage(
              source, "Model part " + modelPartType + " not supported by NPC " + uuid + ".");
          continue;
        }
        modelPartChange.update().apply(modelData, modelPartType, x, y, z);
        modelData.updateModelPose(modelPartType);
        changed++;
      } else {
        sendFailureMessage(
            source,
            "You are not allowed to " + modelPartChange.action() + " the Easy NPC " + uuid + " !");
      }
    }

    if (changed == 0) {
      return sendFailureMessage(source, "Nothing was " + modelPartChange.pastTense() + ".");
    }

    String changedNPCs;
    if (changed == 1) {
      changedNPCs = "NPC " + easyNPCs.iterator().next().getEntity().getDisplayName().getString();
    } else {
      changedNPCs = changed + " of " + easyNPCs.size() + " selected Easy NPCs";
    }
    return sendSuccessMessage(
        source,
        modelPartChange.capitalizedPastTense()
            + " model part '"
            + modelPartType.name().toLowerCase(Locale.ROOT)
            + "' of "
            + changedNPCs
            + " to x:"
            + x
            + ", y:"
            + y
            + ", z:"
            + z);
  }

  @FunctionalInterface
  interface ModelPartUpdate {
    void apply(
        ModelDataCapable<?> modelData, ModelPartType modelPartType, float x, float y, float z);
  }

  record ModelPartChange(String action, String pastTense, ModelPartUpdate update) {

    String capitalizedPastTense() {
      return this.pastTense.substring(0, 1).toUpperCase(Locale.ROOT) + this.pastTense.substring(1);
    }
  }
}
