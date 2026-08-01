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
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class StateDataSet {

  public static final String DATA_STATE_DATA_SET_TAG = "NPCState";
  public static final int MAX_STATE_ENTRIES = 64;
  public static final Codec<Map<Identifier, StateEntry>> CODEC =
      Codec.unboundedMap(Identifier.CODEC, StateEntry.CODEC);
  public static final StreamCodec<RegistryFriendlyByteBuf, StateDataSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public StateDataSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new StateDataSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, StateDataSet stateDataSet) {
          registryFriendlyByteBuf.writeNbt(
              EntityDataSerializersManager.validateAndGetNbt(
                  stateDataSet.createTag(), "StateDataSet"));
        }
      };

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final Map<Identifier, StateEntry> stateEntries = new LinkedHashMap<>();

  public StateDataSet() {}

  public StateDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public StateEntry get(Identifier stateId) {
    return stateId != null ? this.stateEntries.get(stateId) : null;
  }

  public void set(Identifier stateId, StateEntry stateEntry) {
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

  public void remove(Identifier stateId) {
    this.set(stateId, null);
  }

  public boolean has(Identifier stateId) {
    return this.get(stateId) != null;
  }

  public Set<Identifier> keys() {
    return Collections.unmodifiableSet(this.stateEntries.keySet());
  }

  public boolean isEmpty() {
    return this.stateEntries.isEmpty();
  }

  public void clear() {
    this.stateEntries.clear();
  }

  public void load(CompoundTag compoundTag) {
    this.clear();
    if (compoundTag == null || !compoundTag.contains(DATA_STATE_DATA_SET_TAG)) {
      return;
    }

    CODEC
        .parse(NbtOps.INSTANCE, compoundTag.getCompoundOrEmpty(DATA_STATE_DATA_SET_TAG))
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
