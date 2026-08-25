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

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.nio.charset.StandardCharsets;
import java.util.TreeSet;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record ActionDataEntry(
    UUID id,
    ActionDataType actionDataType,
    ConditionDataSet conditionDataSet,
    String command,
    UUID targetUUID,
    BlockPos blockPos,
    boolean executeAsUser,
    boolean enableDebug,
    int permissionLevel,
    MessageActionData messageActionData,
    String poseId,
    ModelAnimationActionData modelAnimationActionData,
    SoundActionData soundActionData,
    MoveActionData moveActionData) {

  public static final String DATA_ID_TAG = "Id";
  public static final String DATA_MESSAGE_TAG = "Msg";
  public static final String DATA_SOUND_TAG = "Snd";
  public static final String DATA_MOVE_TAG = "Move";
  public static final String DATA_POSE_TAG = "Pose";
  public static final String DATA_ANIMATION_TAG = "Anim";
  public static final String DATA_COMMAND_TAG = "Cmd";
  public static final String DATA_DEBUG_TAG = "Debug";
  public static final String DATA_EXECUTE_AS_USER_TAG = "ExecAsUser";
  public static final String DATA_PERMISSION_LEVEL_TAG = "PermLevel";
  public static final String DATA_BLOCK_POS_TAG = "BlockPos";
  public static final String DATA_TARGET_UUID_TAG = "TargetUUID";
  public static final String DATA_TYPE_TAG = "Type";
  public static final int DEFAULT_PERMISSION_LEVEL =
      CommandPermissionLevel.GAMEMASTERS.minecraftLevel();
  public static final int MAX_PERMISSION_LEVEL = CommandPermissionLevel.OWNERS.minecraftLevel();
  public static final int MIN_PERMISSION_LEVEL = CommandPermissionLevel.ALL.minecraftLevel();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DEFAULT_COMMAND = "";

  public ActionDataEntry {
    poseId = poseId == null ? "" : poseId.trim();
    messageActionData = messageActionData != null ? messageActionData : MessageActionData.DEFAULT;
    modelAnimationActionData =
        modelAnimationActionData != null
            ? modelAnimationActionData
            : ModelAnimationActionData.DEFAULT;
    soundActionData = soundActionData != null ? soundActionData : SoundActionData.DEFAULT;
    moveActionData = moveActionData != null ? moveActionData : MoveActionData.DEFAULT;
  }

  public ActionDataEntry(
      UUID id,
      ActionDataType actionDataType,
      ConditionDataSet conditionDataSet,
      String command,
      UUID targetUUID,
      BlockPos blockPos,
      boolean executeAsUser,
      boolean enableDebug,
      int permissionLevel,
      MessageActionData messageActionData) {
    this(
        id,
        actionDataType,
        conditionDataSet,
        command,
        targetUUID,
        blockPos,
        executeAsUser,
        enableDebug,
        permissionLevel,
        messageActionData,
        "",
        ModelAnimationActionData.DEFAULT,
        SoundActionData.DEFAULT,
        MoveActionData.DEFAULT);
  }

  public ActionDataEntry() {
    this(ActionDataType.COMMAND);
  }

  public ActionDataEntry(CompoundTag compoundTag) {
    this(compoundTag, UUID.randomUUID());
  }

  public ActionDataEntry(CompoundTag compoundTag, int position) {
    this(compoundTag, deriveId(compoundTag, position));
  }

  private ActionDataEntry(CompoundTag compoundTag, UUID fallbackId) {
    this(
        compoundTag.contains(DATA_ID_TAG) ? compoundTag.getUUID(DATA_ID_TAG) : fallbackId,
        ActionDataType.get(compoundTag.getString(DATA_TYPE_TAG)),
        compoundTag.contains(ConditionDataSet.CONDITION_DATA_SET_TAG)
            ? new ConditionDataSet(compoundTag.getCompound(ConditionDataSet.CONDITION_DATA_SET_TAG))
            : new ConditionDataSet(),
        compoundTag.contains(DATA_COMMAND_TAG)
            ? compoundTag.getString(DATA_COMMAND_TAG)
            : DEFAULT_COMMAND,
        compoundTag.contains(DATA_TARGET_UUID_TAG)
            ? compoundTag.getUUID(DATA_TARGET_UUID_TAG)
            : null,
        compoundTag.contains(DATA_BLOCK_POS_TAG)
            ? CompoundTagUtils.readBlockPos(compoundTag.getCompound(DATA_BLOCK_POS_TAG))
            : BlockPos.ZERO,
        compoundTag.contains(DATA_EXECUTE_AS_USER_TAG)
            && compoundTag.getBoolean(DATA_EXECUTE_AS_USER_TAG),
        compoundTag.contains(DATA_DEBUG_TAG) && compoundTag.getBoolean(DATA_DEBUG_TAG),
        compoundTag.contains(DATA_PERMISSION_LEVEL_TAG)
            ? checkPermissionLevel(compoundTag.getInt(DATA_PERMISSION_LEVEL_TAG))
            : DEFAULT_PERMISSION_LEVEL,
        compoundTag.contains(DATA_MESSAGE_TAG)
            ? MessageActionData.fromTag(compoundTag.getCompound(DATA_MESSAGE_TAG))
            : MessageActionData.DEFAULT,
        compoundTag.getString(DATA_POSE_TAG),
        compoundTag.contains(DATA_ANIMATION_TAG)
            ? ModelAnimationActionData.fromTag(compoundTag.getCompound(DATA_ANIMATION_TAG))
            : ModelAnimationActionData.DEFAULT,
        compoundTag.contains(DATA_SOUND_TAG)
            ? SoundActionData.fromTag(compoundTag.getCompound(DATA_SOUND_TAG))
            : SoundActionData.DEFAULT,
        compoundTag.contains(DATA_MOVE_TAG)
            ? MoveActionData.fromTag(compoundTag.getCompound(DATA_MOVE_TAG))
            : MoveActionData.DEFAULT);
  }

  public ActionDataEntry(ActionDataType actionDataType) {
    this(actionDataType, DEFAULT_COMMAND, DEFAULT_PERMISSION_LEVEL);
  }

  public ActionDataEntry(ActionDataType actionDataType, String command) {
    this(actionDataType, command, DEFAULT_PERMISSION_LEVEL);
  }

  public ActionDataEntry(ActionDataType actionDataType, String command, int permissionLevel) {
    this(actionDataType, command, permissionLevel, false);
  }

  public ActionDataEntry(
      ActionDataType actionDataType, String command, int permissionLevel, boolean executeAsUser) {
    this(actionDataType, command, permissionLevel, executeAsUser, false);
  }

  public ActionDataEntry(
      ActionDataType actionDataType, String command, boolean executeAsUser, boolean enableDebug) {
    this(actionDataType, command, DEFAULT_PERMISSION_LEVEL, executeAsUser, enableDebug);
  }

  public ActionDataEntry(
      ActionDataType actionDataType,
      String command,
      int permissionLevel,
      boolean executeAsUser,
      boolean enableDebug) {
    this(
        UUID.randomUUID(),
        actionDataType,
        new ConditionDataSet(),
        command != null ? command : DEFAULT_COMMAND,
        null,
        BlockPos.ZERO,
        executeAsUser,
        enableDebug,
        permissionLevel,
        MessageActionData.DEFAULT,
        "",
        ModelAnimationActionData.DEFAULT,
        SoundActionData.DEFAULT,
        MoveActionData.DEFAULT);
  }

  public ActionDataEntry(ActionDataType actionDataType, UUID targetUUID, String command) {
    this(
        UUID.randomUUID(),
        actionDataType,
        new ConditionDataSet(),
        command,
        targetUUID,
        BlockPos.ZERO,
        false,
        false,
        DEFAULT_PERMISSION_LEVEL,
        MessageActionData.DEFAULT,
        "",
        ModelAnimationActionData.DEFAULT,
        SoundActionData.DEFAULT,
        MoveActionData.DEFAULT);
  }

  public static UUID deriveId(CompoundTag compoundTag, int position) {
    StringBuilder identity = new StringBuilder().append(position).append(':');
    appendIdentity(identity, compoundTag);

    return UUID.nameUUIDFromBytes(identity.toString().getBytes(StandardCharsets.UTF_8));
  }

  private static void appendIdentity(StringBuilder identity, Tag tag) {
    if (tag instanceof CompoundTag compoundTag) {
      identity.append('{');
      for (String key : new TreeSet<>(compoundTag.getAllKeys())) {
        identity.append(key).append('=');
        appendIdentity(identity, compoundTag.get(key));
        identity.append(';');
      }
      identity.append('}');
      return;
    }

    if (tag instanceof ListTag listTag) {
      identity.append('[');
      for (Tag entryTag : listTag) {
        appendIdentity(identity, entryTag);
        identity.append(',');
      }
      identity.append(']');
      return;
    }

    identity.append(tag);
  }

  private static int checkPermissionLevel(int permissionLevel) {
    if (permissionLevel < MIN_PERMISSION_LEVEL) {
      log.warn(
          "Permission level {} is too low, will be set to min. level {}",
          permissionLevel,
          MIN_PERMISSION_LEVEL);
      return MIN_PERMISSION_LEVEL;
    }

    CommandPermissionLevel commandPermissionLevel =
        CommandPermissionLevel.fromMinecraftLevel(permissionLevel);
    CommandPermissionLevel maxPermissionLevel =
        CommandPermissionLevel.fromMinecraftLevel(MAX_PERMISSION_LEVEL);
    if (!maxPermissionLevel.allows(commandPermissionLevel)) {
      log.warn(
          "Permission level {} is too high, will be set to a safe max. level {}",
          permissionLevel,
          MAX_PERMISSION_LEVEL);
      return MAX_PERMISSION_LEVEL;
    }
    return permissionLevel;
  }

  public CommandPermissionLevel commandPermissionLevel() {
    return CommandPermissionLevel.fromMinecraftLevel(this.permissionLevel);
  }

  public ActionDataEntry withId(UUID id) {
    Builder builder = new Builder(this);
    builder.id = id;
    return builder.build();
  }

  public ActionDataEntry withBlockPos(BlockPos blockPos) {
    Builder builder = new Builder(this);
    builder.blockPos = blockPos;
    return builder.build();
  }

  public ActionDataEntry withTargetUUID(UUID targetUUID) {
    Builder builder = new Builder(this);
    builder.targetUUID = targetUUID;
    return builder.build();
  }

  public ActionDataEntry withCommand(String command) {
    Builder builder = new Builder(this);
    builder.command = command != null ? command : DEFAULT_COMMAND;
    return builder.build();
  }

  public ActionDataEntry withConditionDataSet(ConditionDataSet conditionDataSet) {
    Builder builder = new Builder(this);
    builder.conditionDataSet =
        conditionDataSet != null ? conditionDataSet : new ConditionDataSet();
    return builder.build();
  }

  public ActionDataEntry withExecuteAsUser(boolean executeAsUser) {
    Builder builder = new Builder(this);
    builder.executeAsUser = executeAsUser;
    return builder.build();
  }

  public ActionDataEntry withPermissionLevel(int permissionLevel) {
    Builder builder = new Builder(this);
    builder.permissionLevel = checkPermissionLevel(permissionLevel);
    return builder.build();
  }

  public ActionDataEntry withMessageActionData(MessageActionData messageActionData) {
    Builder builder = new Builder(this);
    builder.messageActionData = messageActionData;
    return builder.build();
  }

  public ActionDataEntry withPoseId(String poseId) {
    Builder builder = new Builder(this);
    builder.poseId = poseId;
    return builder.build();
  }

  public ActionDataEntry withModelAnimationActionData(
      ModelAnimationActionData modelAnimationActionData) {
    Builder builder = new Builder(this);
    builder.modelAnimationActionData = modelAnimationActionData;
    return builder.build();
  }

  public ActionDataEntry withSoundActionData(SoundActionData soundActionData) {
    Builder builder = new Builder(this);
    builder.soundActionData = soundActionData;
    return builder.build();
  }

  public ActionDataEntry withMoveActionData(MoveActionData moveActionData) {
    Builder builder = new Builder(this);
    builder.moveActionData = moveActionData;
    return builder.build();
  }

  public String getAction(LivingEntity entity, ServerPlayer serverPlayer) {
    return ActionUtils.parseAction(this.command, entity, serverPlayer);
  }

  public boolean hasCommand() {
    return this.command != null;
  }

  public boolean hasCommandAndNotEmpty() {
    return this.command != null && !this.command.isEmpty();
  }

  public boolean hasBlockPos() {
    return this.blockPos != null && this.blockPos != BlockPos.ZERO;
  }

  public boolean isValid() {
    return this.actionDataType != ActionDataType.NONE && this.hasCommand();
  }

  public boolean isValidAndNotEmpty() {
    if (this.actionDataType == ActionDataType.NONE) {
      return false;
    }

    if (this.actionDataType == ActionDataType.MESSAGE) {
      return this.messageActionData.hasTexts();
    }

    if (this.actionDataType == ActionDataType.SET_POSE) {
      return ResourceLocation.tryParse(this.poseId) != null;
    }

    if (this.actionDataType == ActionDataType.PLAY_ANIMATION) {
      return this.modelAnimationActionData.hasAnimationName();
    }

    if (this.actionDataType == ActionDataType.SOUND) {
      return this.soundActionData.hasSoundId();
    }

    if (this.actionDataType == ActionDataType.WAIT) {
      return WaitDuration.parse(this.command).isValid();
    }

    if (this.actionDataType == ActionDataType.MOVE_TO
        || this.actionDataType == ActionDataType.MOVE_TO_AND_WAIT) {
      return this.moveActionData.hasResolvableTarget(this.blockPos);
    }

    if (this.actionDataType == ActionDataType.SET_OPACITY) {
      return ValueUtils.isNumericValue(
          this.command, DisplayAttributeType.MIN_OPACITY, DisplayAttributeType.MAX_OPACITY);
    }

    return !this.actionDataType.requiresArgument()
        || this.hasCommandAndNotEmpty()
        || this.hasBlockPos();
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putUUID(DATA_ID_TAG, this.id);
    compoundTag.putString(DATA_TYPE_TAG, this.actionDataType.name());

    if (this.targetUUID != null) {
      compoundTag.putUUID(DATA_TARGET_UUID_TAG, this.targetUUID);
    }

    if (this.blockPos != BlockPos.ZERO) {
      compoundTag.put(DATA_BLOCK_POS_TAG, CompoundTagUtils.writeBlockPos(this.blockPos));
    }

    if (this.command != null && !this.command.trim().isEmpty()) {
      compoundTag.putString(DATA_COMMAND_TAG, this.command.trim());
    }

    if (this.executeAsUser) {
      compoundTag.putBoolean(DATA_EXECUTE_AS_USER_TAG, true);
    }

    if (this.enableDebug) {
      compoundTag.putBoolean(DATA_DEBUG_TAG, true);
    }

    if (this.permissionLevel != DEFAULT_PERMISSION_LEVEL) {
      compoundTag.putInt(DATA_PERMISSION_LEVEL_TAG, this.permissionLevel);
    }

    if (this.actionDataType == ActionDataType.MESSAGE
        && !this.messageActionData.equals(MessageActionData.DEFAULT)) {
      compoundTag.put(DATA_MESSAGE_TAG, this.messageActionData.createTag());
    }

    if (this.actionDataType == ActionDataType.SOUND
        && !this.soundActionData.equals(SoundActionData.DEFAULT)) {
      compoundTag.put(DATA_SOUND_TAG, this.soundActionData.createTag());
    }

    if ((this.actionDataType == ActionDataType.MOVE_TO
            || this.actionDataType == ActionDataType.MOVE_TO_AND_WAIT)
        && !this.moveActionData.equals(MoveActionData.DEFAULT)) {
      compoundTag.put(DATA_MOVE_TAG, this.moveActionData.createTag());
    }

    if (this.actionDataType == ActionDataType.SET_POSE && !this.poseId.isBlank()) {
      compoundTag.putString(DATA_POSE_TAG, this.poseId.trim());
    }

    if (this.actionDataType == ActionDataType.PLAY_ANIMATION
        || this.actionDataType == ActionDataType.STOP_ANIMATION) {
      CompoundTag animationTag = this.modelAnimationActionData.createTag();
      if (!animationTag.isEmpty()) {
        compoundTag.put(DATA_ANIMATION_TAG, animationTag);
      }
    }

    if (!this.conditionDataSet.isEmpty()) {
      this.conditionDataSet.save(compoundTag);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.write(new CompoundTag());
  }

  public CompoundTag createTag(int position) {
    CompoundTag compoundTag = this.write(new CompoundTag());
    compoundTag.remove(DATA_ID_TAG);

    if (!this.id.equals(deriveId(compoundTag, position))) {
      compoundTag.putUUID(DATA_ID_TAG, this.id);
    }

    return compoundTag;
  }

  private static final class Builder {

    private UUID id;
    private ActionDataType actionDataType;
    private ConditionDataSet conditionDataSet;
    private String command;
    private UUID targetUUID;
    private BlockPos blockPos;
    private boolean executeAsUser;
    private boolean enableDebug;
    private int permissionLevel;
    private MessageActionData messageActionData;
    private String poseId;
    private ModelAnimationActionData modelAnimationActionData;
    private SoundActionData soundActionData;
    private MoveActionData moveActionData;

    private Builder(ActionDataEntry actionDataEntry) {
      this.id = actionDataEntry.id;
      this.actionDataType = actionDataEntry.actionDataType;
      this.conditionDataSet = actionDataEntry.conditionDataSet;
      this.command = actionDataEntry.command;
      this.targetUUID = actionDataEntry.targetUUID;
      this.blockPos = actionDataEntry.blockPos;
      this.executeAsUser = actionDataEntry.executeAsUser;
      this.enableDebug = actionDataEntry.enableDebug;
      this.permissionLevel = actionDataEntry.permissionLevel;
      this.messageActionData = actionDataEntry.messageActionData;
      this.poseId = actionDataEntry.poseId;
      this.modelAnimationActionData = actionDataEntry.modelAnimationActionData;
      this.soundActionData = actionDataEntry.soundActionData;
      this.moveActionData = actionDataEntry.moveActionData;
    }

    private ActionDataEntry build() {
      return new ActionDataEntry(
          this.id,
          this.actionDataType,
          this.conditionDataSet,
          this.command,
          this.targetUUID,
          this.blockPos,
          this.executeAsUser,
          this.enableDebug,
          this.permissionLevel,
          this.messageActionData,
          this.poseId,
          this.modelAnimationActionData,
          this.soundActionData,
          this.moveActionData);
    }
  }
}
