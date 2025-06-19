package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.Collection;
import java.util.Locale;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class PositionCommand extends Command {

  private PositionCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("position")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.argument(NPC_TARGETS_ARGUMENT, EasyNPCArgument.npc())
                .then(
                    Commands.argument("modelPart", StringArgumentType.word())
                        .suggests(
                            (context, builder) -> {
                              EasyNPC<?> easyNPC =
                                  EasyNPCArgument.getEntityWithAccess(
                                      context, NPC_TARGETS_ARGUMENT);
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
                            Commands.argument("x", FloatArgumentType.floatArg(-3.0F, 3.0F))
                                .then(
                                    Commands.argument("y", FloatArgumentType.floatArg(-3.0F, 3.0F))
                                        .then(
                                            Commands.argument(
                                                    "z", FloatArgumentType.floatArg(-3.0F, 3.0F))
                                                .executes(
                                                    context -> {
                                                      String partString =
                                                          StringArgumentType.getString(
                                                              context, "modelPart");
                                                      float x =
                                                          FloatArgumentType.getFloat(context, "x");
                                                      float y =
                                                          FloatArgumentType.getFloat(context, "y");
                                                      float z =
                                                          FloatArgumentType.getFloat(context, "z");

                                                      ModelPartType modelPartType =
                                                          ModelPartType.get(partString);
                                                      if (modelPartType == ModelPartType.UNKNOWN) {
                                                        return sendFailureMessage(
                                                            context.getSource(),
                                                            "Invalid model part " + partString);
                                                      }
                                                      Collection<? extends EasyNPC<?>> easyNPCs =
                                                          EasyNPCArgument.getEntitiesWithAccess(
                                                              context, NPC_TARGETS_ARGUMENT);

                                                      return moveModelPart(
                                                          context.getSource(),
                                                          easyNPCs,
                                                          modelPartType,
                                                          new CustomPosition(x, y, z));
                                                    }))))));
  }

  private static int moveModelPart(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      ModelPartType modelPartType,
      CustomPosition position) {

    int moved = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID uuid = easyNPC.getEntityUUID();
      if (AccessManager.hasAccess(context, uuid)) {
        ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
        if (!modelData.getModelType().getModelParts().contains(modelPartType)) {
          sendFailureMessage(
              context, "Model part " + modelPartType + " not supported by NPC " + uuid + ".");
          continue;
        }
        modelData.setModelPartPosition(modelPartType, position);
        modelData.setModelPose(modelData.hasChangedModel() ? ModelPose.CUSTOM : ModelPose.DEFAULT);
        moved++;
      } else {
        sendFailureMessage(context, "You are not allowed to move the Easy NPC " + uuid + " !");
      }
    }

    return sendPositionFeedback(
        context,
        easyNPCs,
        moved,
        modelPartType.name().toLowerCase(Locale.ROOT),
        position.x(),
        position.y(),
        position.z());
  }

  private static int sendPositionFeedback(
      CommandSourceStack context,
      Collection<? extends EasyNPC<?>> easyNPCs,
      int moved,
      String partName,
      float x,
      float y,
      float z) {

    if (moved == 1) {
      EasyNPC<?> easyNPC = easyNPCs.iterator().next();
      return sendSuccessMessage(
          context,
          "Moved model part '"
              + partName
              + "' of NPC "
              + easyNPC.getEntity().getDisplayName().getString()
              + " to x:"
              + x
              + ", y:"
              + y
              + ", z:"
              + z);
    } else if (moved > 1) {
      return sendSuccessMessage(
          context,
          "Moved model part '"
              + partName
              + "' of "
              + moved
              + " of "
              + easyNPCs.size()
              + " selected Easy NPCs to x:"
              + x
              + ", y:"
              + y
              + ", z:"
              + z);
    }

    return sendFailureMessage(context, "Nothing was moved.");
  }
}
