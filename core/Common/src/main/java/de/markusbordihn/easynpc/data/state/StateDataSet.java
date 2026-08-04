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

package de.markusbordihn.easynpc.data.state;

import com.mojang.serialization.Codec;
import de.markusbordihn.easynpc.Constants;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class StateDataSet {

  public static final String DATA_STATE_DATA_SET_TAG = "NPCState";
  public static final int MAX_STATE_ENTRIES = 64;
  public static final Codec<Map<ResourceLocation, StateEntry>> CODEC =
      Codec.unboundedMap(ResourceLocation.CODEC, StateEntry.CODEC);

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final Map<ResourceLocation, StateEntry> stateEntries = new LinkedHashMap<>();
  private boolean actionEventInProgress;
  private long lastActionEventTick = Long.MIN_VALUE;

  public StateDataSet() {}

  public StateDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public StateEntry get(ResourceLocation stateId) {
    return stateId != null ? this.stateEntries.get(stateId) : null;
  }

  public void set(ResourceLocation stateId, StateEntry stateEntry) {
    if (stateId == null) {
      return;
    }

    if (stateEntry == null) {
      this.stateEntries.remove(stateId);
      return;
    }

    if (!this.stateEntries.containsKey(stateId) && this.stateEntries.size() >= MAX_STATE_ENTRIES) {
      log.error(
          "Unable to set state {}, the limit of {} states per NPC is reached",
          stateId,
          MAX_STATE_ENTRIES);
      return;
    }

    this.stateEntries.put(stateId, stateEntry);
  }

  public void remove(ResourceLocation stateId) {
    this.set(stateId, null);
  }

  public boolean has(ResourceLocation stateId) {
    return this.get(stateId) != null;
  }

  public Set<ResourceLocation> keys() {
    return Collections.unmodifiableSet(this.stateEntries.keySet());
  }

  public boolean isEmpty() {
    return this.stateEntries.isEmpty();
  }

  public void clear() {
    this.stateEntries.clear();
  }

  public boolean tryStartActionEvent(long currentTick, long minimumIntervalTicks) {
    if (this.actionEventInProgress) {
      return false;
    }

    if (this.lastActionEventTick != Long.MIN_VALUE
        && currentTick >= this.lastActionEventTick
        && currentTick - this.lastActionEventTick < minimumIntervalTicks) {
      return false;
    }

    this.actionEventInProgress = true;
    this.lastActionEventTick = currentTick;
    return true;
  }

  public void finishActionEvent() {
    this.actionEventInProgress = false;
  }

  public void load(CompoundTag compoundTag) {
    this.clear();
    this.actionEventInProgress = false;
    this.lastActionEventTick = Long.MIN_VALUE;
    if (compoundTag == null || !compoundTag.contains(DATA_STATE_DATA_SET_TAG)) {
      return;
    }

    CODEC
        .parse(NbtOps.INSTANCE, compoundTag.getCompound(DATA_STATE_DATA_SET_TAG))
        .resultOrPartial(error -> log.warn("Unable to read NPC state: {}", error))
        .ifPresent(loadedStateEntries -> loadedStateEntries.forEach(this::set));
  }

  public CompoundTag save(CompoundTag compoundTag) {
    if (this.stateEntries.isEmpty()) {
      return compoundTag;
    }

    CODEC
        .encodeStart(NbtOps.INSTANCE, this.stateEntries)
        .resultOrPartial(error -> log.warn("Unable to write NPC state: {}", error))
        .ifPresent(stateTag -> compoundTag.put(DATA_STATE_DATA_SET_TAG, stateTag));
    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "StateDataSet [" + this.stateEntries + "]";
  }
}
