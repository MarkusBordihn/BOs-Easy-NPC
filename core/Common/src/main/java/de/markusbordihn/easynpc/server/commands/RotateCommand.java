package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.Collection;
import java.util.Locale;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class RotateCommand extends Command {

  private RotateCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("rotate")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        // Ganzkörper-Rotation
        .then(
            Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc())
                .then(
                    Commands.argument(YAW_ARG, FloatArgumentType.floatArg(0.0F, 360.0F))
                        .executes(
                            context -> {
                              float yaw = FloatArgumentType.getFloat(context, YAW_ARG);
                              Collection<? extends EasyNPC<?>> easyNPCs =
                                  EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG);
                              return rotateEntity(context.getSource(), easyNPCs, yaw);
                            })))
        // Einzelteil-Rotation (ModelPartType)
        .then(
            Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc())
                .then(
                    Commands.argument(MODEL_PART_ARG, StringArgumentType.word())
                        .suggests(
                            (context, builder) -> {
                              EasyNPC<?> easyNPC =
                                  EasyNPCArgument.getEntityWithAccess(context, NPC_TARGETS_ARG);
                              if (easyNPC != null) {
                                ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
                                for (ModelPartType partType :
                                    modelData.getModelType().getModelParts()) {
                                  builder.suggest(partType.name().toLowerCase(Locale.ROOT));
                                }
                              }
                              return builder.buildFuture();
                            })
                        .then(
                            Commands.argument(X_ARG, FloatArgumentType.floatArg(-360.0F, 360.0F))
                                .then(
                                    Commands.argument(
                                            Y_ARG, FloatArgumentType.floatArg(-360.0F, 360.0F))
                                        .then(
                                            Commands.argument(
                                                    Z_ARG,
                                                    FloatArgumentType.floatArg(-360.0F, 360.0F))
                                                .executes(
                                                    context -> {
                                                      String partString =
                                                          StringArgumentType.getString(
                                                              context, MODEL_PART_ARG);
                                                      float x =
                                                          FloatArgumentType.getFloat(
                                                              context, X_ARG);
                                                      float y =
                                                          FloatArgumentType.getFloat(
                                                              context, Y_ARG);
                                                      float z =
                                                          FloatArgumentType.getFloat(
                                                              context, Z_ARG);

                                                      ModelPartType modelPartType =
                                                          ModelPartType.get(partString);
                                                      if (modelPartType == ModelPartType.UNKNOWN) {
                                                        return sendFailureMessage(
                                                            context.getSource(),
                                                            "Invalid model part " + partString);
                                                      }

                                                      Collection<? extends EasyNPC<?>> easyNPCs =
                                                          EasyNPCArgument.getEntitiesWithAccess(
                                                              context, NPC_TARGETS_ARG);

                                                      return rotateModelPart(
                                                          context.getSource(),
                                                          easyNPCs,
                                                          modelPartType,
                                                          new CustomRotation(x, y, z));
                                                    }))))));
  }

  private static int rotateEntity(
      CommandSourceStack context, Collection<? extends EasyNPC<?>> easyNPCs, float yaw) {
    int rotated = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getEntityUUID();
      if (AccessManager.hasAccess(context, uuid)) {
        ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
        if (modelData != null) {
          modelData.setModelRotation(yaw);
        }
        rotated++;
      } else {
        sendFailureMessage(context, "You are not allowed to rotate the Easy NPC " + uuid + " !");
      }
    }

    return sendRotationFeedback(context, easyNPCs, rotated, "body", yaw, 0, 0);
  }

  private static int rotateModelPart(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      ModelPartType modelPartType,
      CustomRotation rotation) {

    int rotated = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getEntityUUID();
      if (AccessManager.hasAccess(context, uuid)) {
        ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
        if (!modelData.getModelType().getModelParts().contains(modelPartType)) {
          sendFailureMessage(
              context, "Model part " + modelPartType + " not supported by NPC " + uuid + ".");
          continue;
        }
        modelData.setModelPartRotation(modelPartType, rotation);
        modelData.setModelPose(modelData.hasChangedModel() ? ModelPose.CUSTOM : ModelPose.DEFAULT);
        rotated++;
      } else {
        sendFailureMessage(context, "You are not allowed to rotate the Easy NPC " + uuid + " !");
      }
    }

    return sendRotationFeedback(
        context,
        easyNPCs,
        rotated,
        modelPartType.name().toLowerCase(Locale.ROOT),
        rotation.x(),
        rotation.y(),
        rotation.z());
  }

  private static int sendRotationFeedback(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      int rotated,
      String partName,
      float x,
      float y,
      float z) {

    if (rotated == 1) {
      EasyNPC<?> easyNPC = easyNPCs.iterator().next();
      return sendSuccessMessage(
          context,
          "Rotated model part '"
              + partName
              + "' of NPC "
              + easyNPC.getEntity().getDisplayName().getString()
              + " to x:"
              + x
              + ", y:"
              + y
              + ", z:"
              + z);
    } else if (rotated > 1) {
      return sendSuccessMessage(
          context,
          "Rotated model part '"
              + partName
              + "' of "
              + rotated
              + " of "
              + easyNPCs.size()
              + " selected Easy NPCs to x:"
              + x
              + ", y:"
              + y
              + ", z:"
              + z);
    }

    return sendFailureMessage(context, "Nothing was rotated.");
  }
}
