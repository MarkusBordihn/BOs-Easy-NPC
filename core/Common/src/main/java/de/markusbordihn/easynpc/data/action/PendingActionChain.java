/*
 * Copyright 2026 Markus Bordihn
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
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record PendingActionChain(
    ActionEventType actionEventType,
    ResourceLocation sourceId,
    int remainingTicks,
    UUID initiatorUUID,
    List<ActionDataEntry> remainingActions,
    List<ActionDataEntry> fallbackActions,
    ActionExecutionState executionState) {

  public static final int MAX_CHAIN_LENGTH = 32;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_EVENT_TYPE_TAG = "Event";
  private static final String DATA_SOURCE_TAG = "Source";
  private static final String DATA_REMAINING_TICKS_TAG = "Ticks";
  private static final String DATA_INITIATOR_TAG = "Initiator";
  private static final String DATA_REMAINING_ACTIONS_TAG = "Actions";
  private static final String DATA_FALLBACK_ACTIONS_TAG = "Fallback";
  private static final String DATA_EXECUTION_STATE_TAG = "State";

  public PendingActionChain {
    actionEventType = actionEventType != null ? actionEventType : ActionEventType.NONE;
    remainingTicks = Math.min(WaitDuration.MAX_TICKS, Math.max(0, remainingTicks));
    remainingActions = limit(remainingActions);
    fallbackActions = limit(fallbackActions);
    executionState = executionState != null ? executionState : ActionExecutionState.EMPTY;
  }

  public PendingActionChain(
      ActionEventType actionEventType,
      int remainingTicks,
      UUID initiatorUUID,
      List<ActionDataEntry> remainingActions,
      List<ActionDataEntry> fallbackActions,
      ActionExecutionState executionState) {
    this(
        actionEventType,
        null,
        remainingTicks,
        initiatorUUID,
        remainingActions,
        fallbackActions,
        executionState);
  }

  public static PendingActionChain fromTag(CompoundTag compoundTag) {
    if (compoundTag == null) {
      return null;
    }

    return new PendingActionChain(
        ActionEventType.get(compoundTag.getString(DATA_EVENT_TYPE_TAG)),
        readSourceId(compoundTag),
        compoundTag.getInt(DATA_REMAINING_TICKS_TAG),
        compoundTag.contains(DATA_INITIATOR_TAG) ? compoundTag.getUUID(DATA_INITIATOR_TAG) : null,
        readActions(compoundTag, DATA_REMAINING_ACTIONS_TAG),
        readActions(compoundTag, DATA_FALLBACK_ACTIONS_TAG),
        ActionExecutionState.fromTag(compoundTag.getCompound(DATA_EXECUTION_STATE_TAG)));
  }

  private static ResourceLocation readSourceId(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_SOURCE_TAG)) {
      return null;
    }

    return ResourceLocation.tryParse(compoundTag.getString(DATA_SOURCE_TAG));
  }

  private static List<ActionDataEntry> limit(List<ActionDataEntry> actionDataEntries) {
    if (actionDataEntries == null || actionDataEntries.isEmpty()) {
      return List.of();
    }

    if (actionDataEntries.size() > MAX_CHAIN_LENGTH) {
      log.warn(
          "Truncating pending action chain from {} to {} actions!",
          actionDataEntries.size(),
          MAX_CHAIN_LENGTH);
      return List.copyOf(actionDataEntries.subList(0, MAX_CHAIN_LENGTH));
    }

    return List.copyOf(actionDataEntries);
  }

  private static List<ActionDataEntry> readActions(CompoundTag compoundTag, String listName) {
    if (!compoundTag.contains(listName)) {
      return List.of();
    }

    ListTag listTag = compoundTag.getList(listName, Tag.TAG_COMPOUND);
    List<ActionDataEntry> actionDataEntries = new ArrayList<>(listTag.size());
    for (int i = 0; i < listTag.size(); i++) {
      actionDataEntries.add(new ActionDataEntry(listTag.getCompound(i)));
    }

    return actionDataEntries;
  }

  private static void writeActions(
      CompoundTag compoundTag, String listName, List<ActionDataEntry> actionDataEntries) {
    if (actionDataEntries.isEmpty()) {
      return;
    }

    ListTag listTag = new ListTag();
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      listTag.add(actionDataEntry.createTag());
    }
    compoundTag.put(listName, listTag);
  }

  public boolean isDue() {
    return this.remainingTicks <= 0;
  }

  public PendingActionChain tick() {
    return new PendingActionChain(
        this.actionEventType,
        this.sourceId,
        this.remainingTicks - 1,
        this.initiatorUUID,
        this.remainingActions,
        this.fallbackActions,
        this.executionState);
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(DATA_EVENT_TYPE_TAG, this.actionEventType.name());
    compoundTag.putInt(DATA_REMAINING_TICKS_TAG, this.remainingTicks);

    if (this.sourceId != null) {
      compoundTag.putString(DATA_SOURCE_TAG, this.sourceId.toString());
    }

    if (this.initiatorUUID != null) {
      compoundTag.putUUID(DATA_INITIATOR_TAG, this.initiatorUUID);
    }

    writeActions(compoundTag, DATA_REMAINING_ACTIONS_TAG, this.remainingActions);
    writeActions(compoundTag, DATA_FALLBACK_ACTIONS_TAG, this.fallbackActions);

    CompoundTag executionStateTag = this.executionState.createTag();
    if (!executionStateTag.isEmpty()) {
      compoundTag.put(DATA_EXECUTION_STATE_TAG, executionStateTag);
    }

    return compoundTag;
  }
}
