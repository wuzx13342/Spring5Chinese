/*
 * Copyright 2002-2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.core.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.rules.TemporaryFolder;

import org.springframework.util.FileCopyUtils;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;
import static org.mockito.BDDMockito.*;

/**
 * Unit tests for the {@link PathResource} class.
 *
 * @author Philippe Marschall
 * @author Phillip Webb
 * @author Nicholas Williams
 * @author Stephane Nicoll
 * @author Juergen Hoeller
 * @author Arjen Poutsma
 */
public class PathResourceTests {

	private static final String TEST_DIR =
			platformPath("src/test/resources/org/springframework/core/io");

	private static final String TEST_FILE =
			platformPath("src/test/resources/org/springframework/core/io/example.properties");

	private static final String NON_EXISTING_FILE =
			platformPath("src/test/resources/org/springframework/core/io/doesnotexist.properties");


	private static String platformPath(String string) {
		return string.replace('/', File.separatorChar);
	}


	@Rule
	public ExpectedException thrown = ExpectedException.none();

	@Rule
	public TemporaryFolder temporaryFolder = new TemporaryFolder();


	@Test
	public void nullPath() throws Exception {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("Path must not be null");
		new PathResource((Path) null);
	}

	@Test
	public void nullPathString() throws Exception {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("Path must not be null");
		new PathResource((String) null);
	}

	@Test
	public void nullUri() throws Exception {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage("URI must not be null");
		new PathResource((URI) null);
	}

	@Test
	public void createFromPath() throws Exception {
		Path path = Paths.get(TEST_FILE);
		PathResource resource = new PathResource(path);
		assertThat(resource.getPath(), equalTo(TEST_FILE));
	}

	@Test
	public void createFromString() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.getPath(), equalTo(TEST_FILE));
	}

	@Test
	public void createFromUri() throws Exception {
		File file = new File(TEST_FILE);
		PathResource resource = new PathResource(file.toURI());
		assertThat(resource.getPath(), equalTo(file.getAbsoluteFile().toString()));
	}

	@Test
	public void getPathForFile() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.getPath(), equalTo(TEST_FILE));
	}

	@Test
	public void getPathForDir() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		assertThat(resource.getPath(), equalTo(TEST_DIR));
	}

	@Test
	public void fileExists() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.exists(), equalTo(true));
	}

	@Test
	public void dirExists() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		assertThat(resource.exists(), equalTo(true));
	}

	@Test
	public void fileDoesNotExist() throws Exception {
		PathResource resource = new PathResource(NON_EXISTING_FILE);
		assertThat(resource.exists(), equalTo(false));
	}

	@Test
	public void fileIsReadable() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.isReadable(), equalTo(true));
	}

	@Test
	public void doesNotExistIsNotReadable() throws Exception {
		PathResource resource = new PathResource(NON_EXISTING_FILE);
		assertThat(resource.isReadable(), equalTo(false));
	}

	@Test
	public void directoryIsNotReadable() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		assertThat(resource.isReadable(), equalTo(false));
	}

	@Test
	public void getInputStream() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		byte[] bytes = FileCopyUtils.copyToByteArray(resource.getInputStream());
		assertThat(bytes.length, greaterThan(0));
	}

	@Test
	public void getInputStreamForDir() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		thrown.expect(FileNotFoundException.class);
		resource.getInputStream();
	}

	@Test
	public void getInputStreamDoesNotExist() throws Exception {
		PathResource resource = new PathResource(NON_EXISTING_FILE);
		thrown.expect(FileNotFoundException.class);
		resource.getInputStream();
	}

	@Test
	public void getUrl() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.getURL().toString(), Matchers.endsWith("core/io/example.properties"));
	}

	@Test
	public void getUri() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.getURI().toString(), Matchers.endsWith("core/io/example.properties"));
	}

	@Test
	public void getFile() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		File file = new File(TEST_FILE);
		assertThat(resource.getFile().getAbsoluteFile(), equalTo(file.getAbsoluteFile()));
	}

	@Test
	public void getFileUnsupported() throws Exception {
		Path path = mock(Path.class);
		given(path.normalize()).willReturn(path);
		given(path.toFile()).willThrow(new UnsupportedOperationException());
		PathResource resource = new PathResource(path);
		thrown.expect(FileNotFoundException.class);
		resource.getFile();
	}

	@Test
	public void contentLength() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		File file = new File(TEST_FILE);
		assertThat(resource.contentLength(), equalTo(file.length()));
	}

	@Test
	public void contentLengthForDirectory() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		File file = new File(TEST_DIR);
		assertThat(resource.contentLength(), equalTo(file.length()));
	}

	@Test
	public void lastModified() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		File file = new File(TEST_FILE);
		assertThat(resource.lastModified() / 1000, equalTo(file.lastModified() / 1000));
	}

	@Test
	public void createRelativeFromDir() throws Exception {
		Resource resource = new PathResource(TEST_DIR).createRelative("example.properties");
		assertThat(resource, equalTo((Resource) new PathResource(TEST_FILE)));
	}

	@Test
	public void createRelativeFromFile() throws Exception {
		Resource resource = new PathResource(TEST_FILE).createRelative("../example.properties");
		assertThat(resource, equalTo((Resource) new PathResource(TEST_FILE)));
	}

	@Test
	public void filename() throws Exception {
		Resource resource = new PathResource(TEST_FILE);
		assertThat(resource.getFilename(), equalTo("example.properties"));
	}

	@Test
	public void description() throws Exception {
		Resource resource = new PathResource(TEST_FILE);
		assertThat(resource.getDescription(), containsString("path ["));
		assertThat(resource.getDescription(), containsString(TEST_FILE));
	}

	@Test
	public void fileIsWritable() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		assertThat(resource.isWritable(), equalTo(true));
	}

	@Test
	public void directoryIsNotWritable() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		assertThat(resource.isWritable(), equalTo(false));
	}

	@Test
	public void outputStream() throws Exception {
		PathResource resource = new PathResource(temporaryFolder.newFile("test").toPath());
		FileCopyUtils.copy("test".getBytes(StandardCharsets.UTF_8), resource.getOutputStream());
		assertThat(resource.contentLength(), equalTo(4L));
	}

	@Test
	public void doesNotExistOutputStream() throws Exception {
		File file = temporaryFolder.newFile("test");
		file.delete();
		PathResource resource = new PathResource(file.toPath());
		FileCopyUtils.copy("test".getBytes(), resource.getOutputStream());
		assertThat(resource.contentLength(), equalTo(4L));
	}

	@Test
	public void directoryOutputStream() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		thrown.expect(FileNotFoundException.class);
		resource.getOutputStream();
	}

	@Test
	public void getReadableByteChannel() throws Exception {
		PathResource resource = new PathResource(TEST_FILE);
		ReadableByteChannel channel = null;
		try {
			channel = resource.readableChannel();
			ByteBuffer buffer = ByteBuffer.allocate((int) resource.contentLength());
			channel.read(buffer);
			buffer.rewind();
			assertThat(buffer.limit(), greaterThan(0));
		}
		finally {
			if (channel != null) {
				channel.close();
			}
		}
	}

	@Test
	public void getReadableByteChannelForDir() throws Exception {
		PathResource resource = new PathResource(TEST_DIR);
		try {
			resource.readableChannel();
		}
		catch (AccessDeniedException ex) {
			// on Windows
		}
	}

	@Test
	public void getReadableByteChannelDoesNotExist() throws Exception {
		PathResource resource = new PathResource(NON_EXISTING_FILE);
		thrown.expect(NoSuchFileException.class);
		resource.readableChannel();
	}

	@Test
	public void getWritableChannel() throws Exception {
		PathResource resource = new PathResource(temporaryFolder.newFile("test").toPath());
		ByteBuffer buffer = ByteBuffer.wrap("test".getBytes(StandardCharsets.UTF_8));
		WritableByteChannel channel = null;
		try {
			channel = resource.writableChannel();
			channel.write(buffer);
		}
		finally {
			if (channel != null) {
				channel.close();
			}
		}
		assertThat(resource.contentLength(), equalTo(4L));
	}

}
